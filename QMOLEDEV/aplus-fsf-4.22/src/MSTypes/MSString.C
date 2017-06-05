///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

extern "C"
{
#include <stdio.h>
#include <float.h>
#include <string.h>
#include <stdlib.h>
#include <memory.h>
}

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSString.H>

#ifndef MSStringBufferHEADER
#include <MSTypes/MSStringBuffer.H>
#endif

#ifndef MSStringTestHEADER
#include <MSTypes/MSStringTest.H>
#endif

#define QUOTE(x) #x
#define STRING(x) QUOTE(x)

#ifndef MSSymbolHEADER
#include <MSTypes/MSSymbol.H>
#endif

#ifdef MS_NO_INLINES
#include <MSTypes/MSStringInlines.C>
#endif

/*------------------------------------------------------------------------------
|Member Name: MSString::null                                                  |
|             MSString::zero                                                  |
|             MSString::maxLong                                               |
|             MSString::hexDigits                                             |
|                                                                             |
| These are static MSString and char* objects used by the MSSring             |
| implementation:                                                             |
|                                                                             |
|   null     -Null(zero length) MSString                                      |
|   zero     -MSString with value=="0"                                        |
|   maxLong  -MSString with ascii representation of LONG_MAX                  |
|   hexDigits-pointer to array of hex digits,in order                         |
------------------------------------------------------------------------------*/

const char *MSString::null="";
const char *MSString::maxLong=STRING(LONG_MAX);

MSModel *MSString::clone(void) const
{ return new MSString(*this); }

MSModel *MSString::create(void) const
{ return new MSString(); }

void MSString::assign(const MSModel& aModel_)
{ *this=(MSString&)aModel_; }

long MSString::compare(const MSModel& aModel_) const
{ return compare((MSString&)aModel_); }

const MSSymbol& MSString::type(void) const
{ return symbol(); }

const MSSymbol& MSString::symbol(void)
{
  static MSSymbol sym ("MSString");
  return sym;
}

//static const char *MSMSF_PROHIBIT=" ,;\'\"-*?!^)}(";  // character prohibited in MSF messages
//static const char *MSMSF_TRANSLATE="\01\02\03\04\05\06\021\022\023\024\025\026\027"; // probited character translations

MSString& MSString::decodeMSF(void)
{ return *this; }

MSString& MSString::encodeMSF(void)
{ return *this; } 

MSString MSString::asMSF(void) const
{
  if (isSet()==MSTrue) return MSString(*this);
  return MSString();
}

MSError::ErrorStatus MSString::setFromMSF(const char *pString_)
{ 
  int code;
  if (pString_!=0) code=set(pString_);
  else code=MSError::BadMSFString;
  return (MSError::ErrorStatus)code;
}

/*------------------------------------------------------------------------------
| MSString::MSString                                                           |
|                                                                              |
| Initialize the string with as accurrate a representation of the input        |
| argument as possible.                                                        |
|                                                                              |
| All constructors(except as noted below) *must* initialize the pBuffer data   |
| member to point to an appropriate null MSStringBuffer object.  This will     |
| cause the resulting MSString to be DBCS-enabled if need be.  This is         |
| accomplished via the static member function MSString::defaultBuffer.  It     |
| will return a pointer to a null Buffer of the appropriate type.              |
|                                                                              |
| The MSString is actually "constructed"(that is,storage buffer allocated and  |
| initialized) via application of the initBuffer function to the just          |
| initialized MSStringBuffer.                                                  |
|                                                                              |
| The copy constructor simply sets the pBuffer data member to point to the     |
| buffer of the argument MSString and bumps that buffer's use count.           |
|                                                                              |
| The special protected constructor that accepts an MSStringBuffer pointer     |
| is another special case.  This constructor is used to pass control of an     |
| MSStringBuffer to another MSString.  It is typically used within non-const   |
| MSString member functions to delay disposal of an MSString's MSStringBuffer  |
| (in cases where that MSStringBuffer may be needed during construction of the |
| new MSString contents).                                                      |
|                                                                              |
| Notes:                                                                       |
|   1. The use count for the null buffer returned by                           |
|      MSString::defaultBuffer is *not* incremented.  This is because          |
|      that buffer(in almost all instances) will be "discarded"(via            |
|      the call to MSString::initBuffer).  The exception is the default        |
|      constructor,which will retain its reference to the null buffer          |
|      and therefore must increment the buffer's use count.                    |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| operator<<                                                                   |
|                                                                              |
| Display the MSString contents on the output stream.                          |
------------------------------------------------------------------------------*/
ostream& operator<<(ostream& aStream,const MSString& aString)
{ return aStream<<aString.data(); }

/*------------------------------------------------------------------------------
| operator >>                                                                  |
|                                                                              |
| Read characters one at a time until a whitespace is reached.                 |
| This method will skip leading white space and then proceed to read           |
| characters from the stream until a white space character is                  |
| encountered. It will leave the terminating white space character on the      |
| stream. This behavior duplicates the behavior of reading char * from a       |
| stream.                                                                      |
------------------------------------------------------------------------------*/
istream &operator>>(istream &aStream_,MSString &aString_)
{
  MSString result(0,128);
  unsigned bytes=0;
  char aChar;

#if (__GNUC__ < 3)
#if defined(MS_IOSTREAM_IPFX_NEEDS_ARGUMENT)
  aStream_.ipfx(0);
#else  
  aStream_.ipfx(); // skip initial white space
#endif
#else
  while (!aStream_.fail()){
    aChar=aStream_.peek();
    if (!isspace(aChar)) break;
  }
#endif
  while (!aStream_.fail())
   {
     aChar=aStream_.peek();
     if (isspace(aChar)) break;
     else
      {
	if (bytes==result.length()) result+=MSString(0,bytes);
        result.data()[bytes++]=aChar; // we know the refCount of result is 1, so this safe
	aStream_.get();
      }
   }
  // Remove trailing stuff...
  aString_=result.remove(bytes);
  return aStream_;
}

/*------------------------------------------------------------------------------
| MSString::lineFrom                                                           |
|                                                                              |
| Read characters up to a delimiter.                                           |
------------------------------------------------------------------------------*/
MSString MSString::lineFrom(istream &aStream_,char delim_)
{
  MSString result(0,128);
  unsigned bytes=0;
  char aChar=delim_;
  
  for (aStream_.get(aChar);aChar!=delim_&&!aStream_.fail();aStream_.get(aChar))
   {
     if (bytes==result.length()) result+=MSString(0,bytes);
     result.data()[bytes++]=aChar; // we know the refCount of result is 1, so this safe
   }
  
  // Remove trailing stuff...
  result.remove(bytes);
  return MSString(result);
}

/*------------------------------------------------------------------------------
| MSString::initBuffer                                                         |
|                                                                              |
| The "base" version of this function simply invokes the corresponding         |
| MSStringBuffer::newBuffer function against the pBuffer member.               |
|                                                                              |
| The long/unsigned long/double functions convert their arguments to strings   |
| and invoke the "base" version.                                               |
------------------------------------------------------------------------------*/
MSString& MSString::initBuffer(const void *p1,unsigned l1,
                               const void *p2,unsigned l2,const void *p3,unsigned l3,
                               char padChar)
{ return setBuffer(_pBuffer->newBuffer(p1,l1,p2,l2,p3,l3,padChar)); }

/*------------------------------------------------------------------------------
| MSString::initBuffer                                                         |
------------------------------------------------------------------------------*/
MSString& MSString::initBuffer(long n)
{
  char buffer[32];
  sprintf(buffer,"%ld",n);
  return initBuffer(buffer,lengthOf(buffer),0,0,0,0,0);
}

/*------------------------------------------------------------------------------
| MSString::initBuffer                                                         |
------------------------------------------------------------------------------*/
MSString& MSString::initBuffer(unsigned long n)
{
  char buffer[32];
  sprintf(buffer,"%lu",n);
  return initBuffer(buffer,lengthOf(buffer),0,0,0,0,0);
}

/*------------------------------------------------------------------------------
| MSString::initBuffer                                                         |
------------------------------------------------------------------------------*/
MSString& MSString::initBuffer(double d)
{
  char buffer[32];
  sprintf(buffer,"%.*g",DBL_DIG,d);
  return initBuffer(buffer,lengthOf(buffer),0,0,0,0,0);
}

/*------------------------------------------------------------------------------
| MSString::initBuffer                                                         |
------------------------------------------------------------------------------*/
MSString& MSString::initBuffer(void *p_)
{
  char buffer[32];
  sprintf(buffer,"%#x",p_);
  return initBuffer(buffer,lengthOf(buffer),0,0,0,0,0);
}

/*------------------------------------------------------------------------------
| MSString::defaultBuffer                                                      |
|                                                                              |
| If DBCS environment is active,return &MSDBCSBuffer::nullBuffer.              |
| Otherwise,return &MSStringBuffer::nullBuffer.                                |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::data                                                               |
|                                                                              |
| Simply return a pointer to the start of the associated MSStringBuffer's buffer.     |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::lengthOf                                                           |
|                                                                              |
| If the argument char* pointer is zero,return 0;  Otherwise,return the        |
| length as calculated by strlen.                                              |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::prepareToChange                                                    |
|                                                                              |
| If the reference count for the referenced MSStringBuffer is greater than one |
|(i.e.,if another MSString is pointing to it),then make a copy.                |
------------------------------------------------------------------------------*/
MSString& MSString::prepareToChange()
{
  if (_pBuffer->useCount()>1)
   {
     MSStringBuffer *oldBuffer=_pBuffer;                            
     initBuffer(data(),length(),0,0,0,0,0);
     oldBuffer->removeRef();
   }
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::rightJustify                                                       |
|                                                                              |
| If the new length differs from the current length,delegate the call to the   |
| MSStringBuffer contents.  The receiver's contents will be replaced with the  |
| new buffer returned.                                                         |
|                                                                              |
------------------------------------------------------------------------------*/
MSString& MSString::rightJustify(unsigned newLength,char padCharacter)
{
  if (length()!=newLength)
   {
     MSStringBuffer *oldBuffer=_pBuffer;                            
     setBuffer(_pBuffer->rightJustify(newLength,padCharacter));
     oldBuffer->removeRef();
   }
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::remove                                                             |
|                                                                              |
| If the number of characters to be removed is non-zero,then delegate this     |
| function via the MSStringBuffer contents.  The receiver's contents are       |
| replaced with the MSStringBuffer returned from that call.                    |
|                                                                              |
------------------------------------------------------------------------------*/
MSString& MSString::remove(unsigned startPos)
{ return remove(startPos,UINT_MAX); }

/*------------------------------------------------------------------------------
| MSString::remove                                                             |
------------------------------------------------------------------------------*/
MSString& MSString::remove(unsigned startPos,unsigned numChars)
{
  if (numChars>0&&startPos<length()&&length()>0)
   {
     MSStringBuffer *oldBuffer=_pBuffer;                            
     setBuffer(_pBuffer->remove(startPos,numChars));
     oldBuffer->removeRef();
   }
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::remove                                                             |
------------------------------------------------------------------------------*/
MSString MSString::remove(const MSString &aString,unsigned startPos,unsigned numChars)
{ return MSString(aString).remove(startPos,numChars); }

/*------------------------------------------------------------------------------
| MSString::setBuffer                                                          |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::asDebugInfo                                                        |
------------------------------------------------------------------------------*/
MSString MSString::asDebugInfo() const
{
  MSString result("MSString(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",pBuffer->";
  result+=_pBuffer->asDebugInfo();
  result+=")";
  return result;
}

/*------------------------------------------------------------------------------
| MSString::className                                                          |
------------------------------------------------------------------------------*/
MSString MSString::className() const
{ return MSString("MSString"); }

MSString& MSString::operator=(const char *pString)
{
  MSStringBuffer *b=_pBuffer;
  setBuffer(b->newBuffer(pString,lengthOf(pString),0,0,0,0,0));
  b->removeRef();
  return *this;
}

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the constructors for the       *
*   class MSString.                                                            *
*******************************************************************************/
MSString::MSString(const void *pBuffer,unsigned len,char padCharacter):_pBuffer(MSString::defaultBuffer())
{ initBuffer(pBuffer,len,0,0,0,0,padCharacter); }

MSString::MSString(const void *pBuffer1,unsigned len1,
		   const void *pBuffer2,unsigned len2,
		   char padCharacter):_pBuffer(MSString::defaultBuffer())
{ initBuffer(pBuffer1,len1,pBuffer2,len2,0,0,padCharacter); }

MSString::MSString(const void *pBuffer1,unsigned len1,
		   const void *pBuffer2,unsigned len2,
		   const void *pBuffer3,unsigned len3,
		   char padCharacter):_pBuffer(MSString::defaultBuffer())
{ initBuffer(pBuffer1,len1,pBuffer2,len2,pBuffer3,len3,padCharacter); }

MSString::MSString(int n):_pBuffer(MSString::defaultBuffer())
{ initBuffer((long) n); }

MSString::MSString(long n):_pBuffer(MSString::defaultBuffer())
{ initBuffer(n); }

MSString::MSString(short n):_pBuffer(MSString::defaultBuffer())
{ initBuffer((long)n); }

MSString::MSString(unsigned n):_pBuffer(MSString::defaultBuffer())
{ initBuffer((unsigned long) n); }

MSString::MSString(unsigned long n):_pBuffer(MSString::defaultBuffer())
{ initBuffer(n); }

MSString::MSString(unsigned short n):_pBuffer(MSString::defaultBuffer())
{ initBuffer((unsigned long)n); }

MSString::MSString(double aDouble):_pBuffer(MSString::defaultBuffer())
{ initBuffer(aDouble); }

MSString::MSString(char aChar):_pBuffer(MSString::defaultBuffer())
{ initBuffer((const void *)&aChar,1,0,0,0,0,0); }

MSString::MSString(unsigned char aChar):_pBuffer(MSString::defaultBuffer())
{ initBuffer((const void *)&aChar,1,0,0,0,0,0); }

MSString::MSString(signed char aChar):_pBuffer(MSString::defaultBuffer())
{ initBuffer((const void *)&aChar,1,0,0,0,0,0); }

MSString::MSString(const char *p):_pBuffer(MSString::defaultBuffer())
{ initBuffer((const void*)p,lengthOf(p),0,0,0,0,0); }

MSString::MSString(const unsigned char *p):_pBuffer(MSString::defaultBuffer())
{ initBuffer((const void*)p,lengthOf((const char *)p),0,0,0,0,0); }

MSString::MSString(const signed char *p):_pBuffer(MSString::defaultBuffer())
{ initBuffer((const void*)p,lengthOf((const char *)p),0,0,0,0,0); }

MSString::MSString(const MSString &aString)
{ setBuffer(aString._pBuffer->addRef()); }

MSString::MSString():_pBuffer(MSString::defaultBuffer())
{ _pBuffer->addRef(); }

MSString::MSString(MSStringBuffer *oldBuffer)
{ setBuffer(oldBuffer); }                                                          

/*------------------------------------------------------------------------------
| MSString::~MSString                                                          |
------------------------------------------------------------------------------*/
MSString::~MSString()
{ _pBuffer->removeRef(),_pBuffer=0; }

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the testing functions of       *
*   the class MSString.                                                        *
*******************************************************************************/
/*------------------------------------------------------------------------------
| MSString::isAlnum                                                            |
| MSString::isAlpha                                                            |
| MSString::isAscii                                                            |
| MSString::isCntrl                                                            |
| MSString::isDigits                                                           |
| MSString::isHexDigits                                                        |
| MSString::isLowerCase                                                        |
| MSString::isPrintable                                                        |
| MSString::isPunctuation                                                      |
| MSString::isUpperCase                                                        |
| MSString::isWhiteSpace                                                       |
|                                                                              |
| Each of these functions is delegated to the associated MSStringBuffer        |
| holding the contents of the receiving MSString.                              |
|                                                                              |
| Notes:                                                                       |
|   1. The default(SBCS) implementation is to utilize the                      |
|      corresponding standard C library function to test the string            |
|      contents.                                                               |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::isBinaryDigits                                                     |
|                                                                              |
| Examine each character of the receiver in turn,returning MSFalse if a non '0'|
| or '1' is found.  Otherwise,return MSTrue.                                   |
|                                                                              |
| The test for '0'/'1' is made by exclusive-or-ing with '1'; this yields all   |
| the bits that differ from '1'.  If any but the least significant vary,then   |
| the test fails.                                                              |
|                                                                              |
| Notes:                                                                       |
|   1. If the string is null(of length 0) then 1(MSTrue) is returned.          |
------------------------------------------------------------------------------*/
MSBoolean MSString::isBinaryDigits() const
{
  const char *p=data();
  unsigned pos=length();
  while (pos--) if ((*p++ ^ '1')>1) return MSFalse;
  return MSTrue;
}

/*------------------------------------------------------------------------------
| MSString::isLike                                                             |
|                                                                              |
| This function is implemented by invoking the protected version that accepts  |
| a char* pointer and length.                                                  |
------------------------------------------------------------------------------*/
MSBoolean MSString::isLike(const char *pPattern,unsigned patternLen,char zeroOrMore,char anyChar) const
{
  // Answer MSTrue if the receiver and pattern are equal.
  if (_pBuffer->compare(pPattern,patternLen)==MSStringBuffer::equal) return MSTrue;
  // Index into strings:
  unsigned iText=0,iPattern=0,lastStar=0,len=length();
  // Continue till we get to the end of the pattern...
  while (iPattern<patternLen)
   {
     char p=pPattern[iPattern++];
     // See if the pattern character matches "zeroOrMore" characters...
     if (p==zeroOrMore)
     // We have reached a*in the pattern.
     // If at the end,then the receiver has matched it(since
     // the last*matches the rest of the text).
     // Otherwise,the scan now starts here(just beyond the *)
     // since the previous portion of the pattern has been
     // matched.
     if (iPattern>=patternLen) return MSTrue;
     else lastStar=iPattern;
     else
     // Not a *.  If we are at the end of the text,then the
     // result is MSFalse(since there is no character to match
     // the next pattern character).
     if (iText>=len) return MSFalse;
     else
      {
        // Compare text/pattern characters...
        char t=data()[iText++];
        if (p==anyChar||p==t)
	// Characters match.  If we had a*somewhere,are at
	// the end of the pattern,and not at the end of the
	// text,then we need to restart the scan from the last
	//*while using it to swallow one more character of
	// the text.
	//
	// This situation arises in cases like the following:
	//     pattern=*abc
	//     text  =123abcabc
	//
	// If these conditions aren't met,then we simply go
	// back to the top of the while loop and process the
	// next pattern character.
	if (lastStar>0&&iPattern>=patternLen&&iText<len);
	else
	continue;
        else
	// Characters don't match.  If we haven't seen a
	// "zeroOrMore" character in the pattern,then we've
	// failed to match it.
	if (lastStar==0) return MSFalse;
        // We get here if we need to "reset" the scan to the
        // last "zeroOrMore" wildcard in the pattern.  First,
        // calculate how many characters were matched after the
        // last such wildcard in the pattern:
        unsigned matched=iPattern-lastStar-1;
        if (matched==0) matched=1;
        // Reset pattern position to just past it:
        iPattern=lastStar;
        // Back up in the text stream "matched" characters and
        // then skip one character(i.e.,let the wildcard swallow
        // one more:
        iText-=(1-matched);
        iText+=(matched-1);
      }
   }
  // At end of pattern.  If also at the end of the text,then they match,
  // otherwise,they don't:
  return MSBoolean(iText>=len);
}

/*------------------------------------------------------------------------------
| MSString::isAbbrevFor                                                        |
|                                                                              |
| This function is implemented via the protected member of the same name.      |
| That function accepts as arguments a full specification of the "full"        |
| string(pointer *and* length) and the minimum abbreviation length.  It        |
| determines whether the receiver is an acceptable abbreviation by comparing   |
| the appreviation to beginning of the full string and if equal,comparing the  |
| length of the abbreviation to the minimum.                                   |
------------------------------------------------------------------------------*/
MSBoolean MSString::isAbbrevFor(const char *pFullString,unsigned fullStringLen,unsigned minLen) const
{
  if (minLen==0) minLen=length();
  if (length()<=fullStringLen&&length()>=minLen&&memcmp(data(),pFullString,length())==0) return MSTrue;
  else return MSFalse;
}

/*------------------------------------------------------------------------------
| MSString::includes                                                           |
|                                                                              |
| Simply invoke the equivalent indexOf function.                               |
|                                                                              |
| Notes:                                                                       |
|   1. These funcions are essentially just shorthand for the corresponding     |
|      indexOf functions.  It provides a more meaningful function name         |
|      in cases where invoked to obtain a MSBoolean result(rather than the     |
|      specific index within the string).                                      |
------------------------------------------------------------------------------*/
// See MSString.H

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the comparison operators for   *
*   the class MSString.                                                        *
*******************************************************************************/
/*------------------------------------------------------------------------------
| operator==                                                                   |
| operator!=                                                                   |
| operator<                                                                    |
| operator<=                                                                   |
| operator>                                                                    |
| operator>=                                                                   |
|                                                                              |
| Compares two MSStrings(or a MSString and a char* string) for the specified   |
| condition by calling MSStringBuffer::compare().                              |
------------------------------------------------------------------------------*/
// See MSString.H
/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the "conversion" functions for *
*   the class MSString.                                                        *
*******************************************************************************/
/*------------------------------------------------------------------------------
| MSString::operator char*                                                     |
| MSString::unsigned char*                                                     |
| MSString::signed   char*                                                     |
| MSString::asInt                                                              |
| MSString::asUnsigned                                                         |
| MSString::asBoolean                                                          |
| MSString::asDouble                                                           |
|                                                                              |
|                                                                              |
| Convert the string contents to the appropriate type,using the appropriate    |
| strto<x> library function.  Conversion to char* simply obtains the address   |
| of the associated buffer data.                                               |
------------------------------------------------------------------------------*/
// See MSString.H

// this is ugly, but the goal is speed, i.e. the most likely hits are first
MSBoolean MSString::asBoolean() const
{ 
  MSString buf(lowerCase(*this));
  if (buf=="0")       return MSFalse;
  if (buf=="1")       return MSTrue;
  if (buf=="false")   return MSFalse;
  if (buf=="true")    return MSTrue;
  if (buf=="no")      return MSFalse;
  if (buf=="yes")     return MSTrue;
  if (buf=="mstrue")  return MSTrue;
  if (buf=="msfalse") return MSFalse;
  return MSFalse;
}

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the MSString manipulation      *
*   operators.                                                                 *
*******************************************************************************/
/*------------------------------------------------------------------------------
| MSString::operator=                                                          |
|                                                                              |
| Assign the argument string to this MSString by copying its contents.  Note   |
| that since MSStrings can be constructed from any basic type,any basic type   |
| can be assigned to an MSString object.                                       |
|                                                                              |
| Notes:                                                                       |
|   1. An MSString is constructed from this object's MSStringBuffer.  This will|
|      ensure the previously referenced MSStringBuffer's reference count gets  |
|      decremented at the appropriate time.  This technique is used            |
|      throughout the MSString implementation; it will rarely be noted         |
|      elsewhere.                                                              |
|                                                                              |
|   2. We add the reference to the source MSStringBuffer prior to removing     |
|      the reference to the receiver's MSStringBuffer(which happens at exit    |
|      during deletion of "old"); this ensures the proper result when          |
|      assigning an MSString to                                                |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::operator ~                                                         |
|                                                                              |
| Create the one's complement of the receiver by negating each byte.           |
------------------------------------------------------------------------------*/
MSString MSString::operator ~() const
{
  unsigned len=length();
  const unsigned char *pSource=(unsigned char *) data();
  MSString result(0,len);
  unsigned char *pDest=(unsigned char *) result.data();
  while (len--) *pDest++=~*pSource++;
  return result;
}

/*------------------------------------------------------------------------------
| MSString::operator+                                                          |
|                                                                              |
| Concatenate the argument string to the receiver.  Each of these is           |
| implemented using the constructor accepting two buffers                      |
|(as pointer/length pairs).                                                    |
|                                                                              |
| Notes:                                                                       |
|   1. These functions(along with the other const operators) return            |
|      the result of the operation in a new MSString.                          |
------------------------------------------------------------------------------*/
MSString MSString::operator+(const MSString &aString) const
{ return MSString(data(),length(),aString.data(),aString.length()); }

MSString MSString::operator+(const char *pString) const
{ return MSString(data(),length(),pString,lengthOf(pString)); }

MSString MSString::operator+(char aChar) const
{ return MSString(data(),length(),&aChar,1); }

MSString operator+(const char *pString,const MSString &aString)
{ return MSString(pString,MSString::lengthOf(pString),(const char*)aString,aString.length()); }

MSString operator+(char aChar,const MSString &aString)
{ return MSString(&aChar,1,(const char*)aString,aString.length()); }

/*------------------------------------------------------------------------------
| MSString::operator+=                                                         |
| MSString::operator<<=                                                        |
|                                                                              |
| Concatenate the argument string to the receiver's contents.  These are all   |
| implemented using the MSStringBuffer->initBuffer function.  If the argument  |
| string is null,simply return the receiver unmodified.                        |
------------------------------------------------------------------------------*/
MSString& MSString::operator+=(const MSString &aString)
{
  if (aString.length()>0)
   {
     MSStringBuffer *oldBuffer=_pBuffer;                            
     initBuffer(data(),length(),aString.data(),aString.length(),0,0,0);
     oldBuffer->removeRef();
   }
  return *this;
}

MSString& MSString::operator+=(const char *pString)
{
  unsigned argLen=lengthOf(pString);
  if (argLen>0)
   {
     MSStringBuffer *oldBuffer=_pBuffer;                            
     initBuffer(data(),length(),pString,argLen,0,0,0);
     oldBuffer->removeRef();
   }
  return *this;
}

MSString& MSString::operator+=(char aChar)
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  initBuffer(data(),length(),&aChar,1,0,0,0);
  oldBuffer->removeRef();
  return *this;
}

MSString& MSString::operator<<=(const MSString& aString) 
{ return (*this)+=aString; }
MSString& MSString::operator<<=(const char *pString)
{ return (*this)+=pString; }
MSString& MSString::operator<<=(char aChar)
{ return (*this)+=aChar; }

MSString& MSString::operator<<(const MSString& aString) 
{ return (*this)+=aString; }
MSString& MSString::operator<<(const char *pString)
{ return (*this)+=pString; }
MSString& MSString::operator<<(char aChar)
{ return (*this)+=aChar; }

/*------------------------------------------------------------------------------
| MSString::operator&                                                          |
| MSString::operator|                                                          |
| MSString::operator ^                                                         |
|                                                                              |
| These functions return a MSString corresponding to the receiver's contents   |
| operated on via one of the bit-wise operators &(and),|(or),or                |
| ^(exclusive or) and the provided argument.  If the argument string is        |
| shorter than the receiver,then that string is replicated in order to         |
| perform the operation.                                                       |
|                                                                              |
| These are all implemented by constructing a result string from the receiver  |
| and invoking the protected member function applyBitOp().  The arguments to   |
| that function are the argument string and the bitOpFunction(either and,or,   |
| or exclusiveOr).                                                             |
------------------------------------------------------------------------------*/
MSString MSString::operator &(const MSString &aString) const
{ return MSString(*this).applyBitOp(aString.data(),aString.length(),And); }

/*------------------------------------------------------------------------------
| MSString::operator&                                                         |
------------------------------------------------------------------------------*/
MSString MSString::operator &(const char *pString) const
{ return MSString(*this).applyBitOp(pString,lengthOf(pString),And); }

/*------------------------------------------------------------------------------
| operator&                                                                    |
------------------------------------------------------------------------------*/
MSString operator &(const char *pString,const MSString &aString)
{ return MSString(pString).applyBitOp(aString.data(),aString.length(),MSString::And); }

/*------------------------------------------------------------------------------
| MSString::operator|                                                          |
------------------------------------------------------------------------------*/
MSString MSString::operator |(const MSString &aString) const
{ return MSString(*this).applyBitOp(aString.data(),aString.length(),Or); }

/*------------------------------------------------------------------------------
| MSString::operator|                                                          |
------------------------------------------------------------------------------*/
MSString MSString::operator |(const char *pString) const
{ return MSString(*this).applyBitOp(pString,lengthOf(pString),Or); }

/*------------------------------------------------------------------------------
| operator|                                                                    |
------------------------------------------------------------------------------*/
MSString operator |(const char *pString,const MSString &aString)
{ return MSString(pString).applyBitOp(aString.data(),aString.length(),MSString::Or); }

/*------------------------------------------------------------------------------
| MSString::operator ^                                                         |
------------------------------------------------------------------------------*/
MSString MSString::operator ^(const MSString &aString) const
{ return MSString(*this).applyBitOp(aString.data(),aString.length(),ExclusiveOr); }

/*------------------------------------------------------------------------------
| MSString::operator ^                                                         |
------------------------------------------------------------------------------*/
MSString MSString::operator ^(const char *pString) const
{ return MSString(*this).applyBitOp(pString,lengthOf(pString),ExclusiveOr); }

/*------------------------------------------------------------------------------
| operator ^                                                                   |
------------------------------------------------------------------------------*/
MSString operator ^(const char *pString,const MSString &aString)
{ return MSString(pString).applyBitOp(aString.data(),aString.length(),MSString::ExclusiveOr); }

/*------------------------------------------------------------------------------
| MSString::operator &=                                                        |
| MSString::operator |=                                                        |
| MSString::operator ^=                                                        |
|                                                                              |
| The receiver's contents are operated on via a bit-wise operator and the      |
| argument string(which can be either a MSString object or a char*).  These    |
| are all implemented by invoking the protected member function applyBitOp()   |
| with the appropriate BitOp(either "And","or",Or "ExclusiveOr."               |
------------------------------------------------------------------------------*/
MSString& MSString::operator &=(const MSString &aString)
{ return applyBitOp(aString.data(),aString.length(),And); }

/*------------------------------------------------------------------------------
| MSString::operator &=                                                        |
------------------------------------------------------------------------------*/
MSString& MSString::operator &=(const char *pString)
{ return applyBitOp(pString,lengthOf(pString),And); }

/*------------------------------------------------------------------------------
| MSString::operator |=                                                        |
------------------------------------------------------------------------------*/
MSString& MSString::operator |=(const MSString &aString)
{ return applyBitOp(aString.data(),aString.length(),Or); }

/*------------------------------------------------------------------------------
| MSString::operator |=                                                        |
------------------------------------------------------------------------------*/
MSString& MSString::operator |=(const char *pString)
{ return applyBitOp(pString,lengthOf(pString),Or); }

/*------------------------------------------------------------------------------
| MSString::operator ^=                                                        |
------------------------------------------------------------------------------*/
MSString& MSString::operator ^=(const MSString &aString)
{ return applyBitOp(aString.data(),aString.length(),ExclusiveOr); }

/*------------------------------------------------------------------------------
| MSString::operator ^=                                                        |
------------------------------------------------------------------------------*/
MSString& MSString::operator ^=(const char *pString)
{ return applyBitOp(pString,lengthOf(pString),ExclusiveOr); }

/*------------------------------------------------------------------------------
| MSString::applyBitOp                                                         |
|                                                                              |
| If the argument is null,then the receiver is returned unmodified.            |
| Otherwise,the receiver is operated on according to the length of             |
| the argument:                                                                |
|                                                                              |
| if 1,then each character of the receiver is operated on with                 |
| the single byte of the argument                                              |
|                                                                              |
| else,the receiver is broken up into chunks the size of the                   |
| argument and then each byte of the chunk is operated on with                 |
| the corresponding byte of the argument.                                      |
|                                                                              |
| To as great an extent as possible,the switch on operator type is             |
| performed outside of the loops that perform the operations.                  |
------------------------------------------------------------------------------*/
MSString& MSString::applyBitOp(const char *pArg,unsigned argLen,BitOperator op)
{
  if (argLen!=0)
   {
     prepareToChange();
     char     *pt=data();
     unsigned  n=length();
     if (argLen==1)
      {
	switch(op)
	 {
	 case And:
	   while (n--) *pt++ &= *pArg;
	   break;
	 case Or:
	   while (n--) *pt++ |= *pArg;
	   break;
	 case ExclusiveOr:
	   while (n--) *pt++ ^= *pArg;
	 }
      }
     else
      {
	while (n)
	 {
	   const char *pa=pArg;
	   unsigned    m =(argLen<n)?argLen:n;
	   n-=m;
	   switch(op)
	    {
	    case And:
	      while (m--) *pt++ &= *pa++;
	      break;
	    case Or:
	      while (m--) *pt++ |= *pa++;
	      break;
	    case ExclusiveOr:
	      while (m--) *pt++ ^= *pa++;
	    }
	 }
      }
   }
  return *this;
}
/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the "accessing" functions for  *
*   the class MSString.                                                        *
*******************************************************************************/
/*------------------------------------------------------------------------------
| MSString::length                                                             |
|                                                                              |
| Simply return the length field from the associated MSStringBuffer.           |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::size                                                               |
|                                                                              |
| This function is simply a synonym for MSString::length.                      |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::subString                                                          |
|                                                                              |
| The result string is constructed using the MSStringBuffer obtained via the   |
| function MSStringBuffer::subString applied to the receiver's MSStringBuffer. |
|                                                                              |
| Notes:                                                                       |
|   1. The result of this function varies for DBCS vs. SBCS strings.           |
|      In the case of DBCS,substringing is accomplished as follows:            |
|        a. First,the specified bytes are(logically) extracted                 |
|           from the string.                                                   |
|        b. If the first byte of this substring is the second byte             |
|           of a DBCS character,then the preceding byte is                     |
|           prepended to the result and the last byte is removed.              |
|        c. If the last byte is the substring is now the first byte            |
|           of a DBCS character,then it is converted to the                    |
|           padCharacter.                                                      |
|      This ensures that the result has length specified by the                |
|      input length argument.                                                  |
------------------------------------------------------------------------------*/
MSString MSString::subString(unsigned startPos) const
{
  if (startPos<length()) return subString(startPos,length()-startPos);
  else return MSString::null;
}

/*------------------------------------------------------------------------------
| MSString::subString                                                          |
------------------------------------------------------------------------------*/
MSString MSString::subString(unsigned startPos,unsigned len,char padCharacter) const
{ return MSString(_pBuffer->subString(startPos,len,padCharacter)); }

MSString MSString::operator()(unsigned startPos,unsigned len,char padCharacter) const
{ return MSString(_pBuffer->subString(startPos,len,padCharacter)); }

MSString MSString::operator()(unsigned startPos,unsigned len) const
{ return MSString(_pBuffer->subString(startPos,len,' ')); }

/*------------------------------------------------------------------------------
| MSString::operator[]                                                         |
|                                                                              |
| The const and non-const version of these functions differ in the way         |
| indexing off the end of the string is handled.  For the const case,an        |
| Invalid request exception is thrown. In the non-const case,the string is     |
| extended with blanks(and a reference to the last blank added is returned).   |
|                                                                              |
| Otherwise,return the specified element of the receiver's buffer.             |
|                                                                              |
| Notes:                                                                       |
|   1. The indexing is 0-based.                                                |
|   2. This function presents some problems for the integrity of MSString      |
|      contents(as does operator char*).  While some precautions are           |
|      taken(ensuring the receiver has the only reference to its               |
|      buffer) subsequent assignment of the MSString and manipulation via      |
|      the char& returned by this function may cause unexpected result.        |
------------------------------------------------------------------------------*/
char MSString::operator()(unsigned index) const
{
  static char overFlow=0;
  return (index<length())?data()[index]:overFlow;
}

char MSString::operator()(unsigned index) 
{
  static char overFlow=0;
  return (index<length())?data()[index]:overFlow;
}

const char& MSString::operator[](unsigned index) const
{
  static char overFlow=0;
  return (index<length())?data()[index]:overFlow;
}

/*------------------------------------------------------------------------------
| MSString::operator []                                                        |
------------------------------------------------------------------------------*/
MSString::CharPick MSString::operator[](unsigned index)
{
  // Test whether buffer has to grow:
  if (index>=_pBuffer->length())
   { // It does,
     MSStringBuffer *oldBuffer=_pBuffer;                            
     initBuffer(oldBuffer->contents(),oldBuffer->length(),0,index-oldBuffer->length()+1,0,0,' ');
     oldBuffer->removeRef();       
   }
  else
   { // It doesn't,but make sure we've got our own copy.
     prepareToChange();
   }
  return MSString::CharPick(this,index);
}

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the searching functions of     *
*   the class MSString.                                                        *
*******************************************************************************/
/*------------------------------------------------------------------------------
| MSString::indexOf                                                            |
| MSString::indexOfAnyBut                                                      |
| MSString::indexOfAnyOf                                                       |
|                                                                              |
| Each of these functions is implemented by delegating to the string's         |
| MSStringBuffer contents.                                                     |
------------------------------------------------------------------------------*/
// See MSString.H

unsigned MSString::indexOf(char aChar,unsigned startPos) const
{ return _pBuffer->indexOf(&aChar,1,startPos); }

/*------------------------------------------------------------------------------
| MSString::lastIndexOf                                                        |
| MSString::lastIndexOfAnyBut                                                  |
| MSString::lastIndexOfAnyOf                                                   |
|                                                                              |
| Each of these functions is implemented by delegating to the string's         |
| MSStringBuffer contents.                                                     |
------------------------------------------------------------------------------*/
// See MSString.H

unsigned MSString::lastIndexOf(char aChar,unsigned startPos) const
{ return _pBuffer->lastIndexOf(&aChar,1,startPos); }

/*------------------------------------------------------------------------------
| MSString::occurrencesOf                                                      |
|                                                                              |
| The public versions of this function that accept char* and char              |
| arguments are implementd via a common protected version that accepts         |
| char* and length.  It in turn implements the function by repetitive          |
| calls to MSStringBuffer::indexOf,keeping count of the number of instances    |
| of the search string that are located.                                       |
|                                                                              |
| The public function accepting an MSStringTest argument implements the        |
| function by repetitive calls to the corresponding MSStringBuffer::indexOf    |
| function,counting the number of characters for which the "test" is           |
| successful.                                                                  |
------------------------------------------------------------------------------*/
unsigned MSString::occurrencesOf(const char *pSearchString,unsigned searchLen,unsigned startPos) const
{
  unsigned count=0;
  while ((startPos=_pBuffer->indexOf(pSearchString,searchLen,startPos))<_pBuffer->length())
   {
     count++;
     startPos+=searchLen;
   }
  return count;
}

/*------------------------------------------------------------------------------
| MSString::occurrencesOf                                                      |
------------------------------------------------------------------------------*/
unsigned MSString::occurrencesOf(const MSStringTest &aTest,unsigned startPos) const
{
  unsigned count=0;
  while ((startPos=_pBuffer->indexOf(aTest,startPos))<_pBuffer->length()) count++,startPos++;
  return count;
}

/*------------------------------------------------------------------------------
| MSString::occurrencesOf                                                      |
------------------------------------------------------------------------------*/
unsigned MSString::occurrencesOf(char searchChar,unsigned startPos) const
{ return occurrencesOf(&searchChar,1,startPos); }

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the formatting functions of    *
*   the class MSString.                                                        *
*******************************************************************************/
/*------------------------------------------------------------------------------
| MSString::format                                                             |
------------------------------------------------------------------------------*/
const char *MSString::format(MSString *pString_) const 
{ *pString_=*this; return pString_->data();;}
const char *MSString::format(MSString& aString_) const 
{ aString_=*this; return aString_.data(); }

const char *MSString::format(MSString *pString_,const MSFormat&) const 
{ *pString_=*this; return pString_->data();;}
const char *MSString::format(MSString& aString_,const MSFormat&) const 
{ aString_=*this; return aString_.data(); }

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the editing functions of       *
*   the class MSString.                                                        *
*******************************************************************************/
/*------------------------------------------------------------------------------
| MSString::set                                                                |
------------------------------------------------------------------------------*/
MSError::ErrorStatus MSString::set(unsigned index,char aChar)
{
  if (index<length())
   {
     prepareToChange();
     data()[index]=aChar;
     changed();
     return MSError::MSSuccess;
   }
  return MSError::MSFailure;
}

MSError::ErrorStatus MSString::set(const char *pString)     
{ *this=pString; return MSError::MSSuccess; }
MSError::ErrorStatus MSString::set(const MSString *pString) 
{ *this=*pString; return MSError::MSSuccess; }
MSError::ErrorStatus MSString::set(const MSString& aString) 
{ *this=aString; return MSError::MSSuccess; }

/*------------------------------------------------------------------------------
| MSString::center                                                             |
------------------------------------------------------------------------------*/
MSString& MSString::center(unsigned newLength,char padCharacter)
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  setBuffer(_pBuffer->center(newLength,padCharacter));
  oldBuffer->removeRef();  
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::center                                                             |
------------------------------------------------------------------------------*/
MSString MSString::center(const MSString &aString,unsigned newLength,char padCharacter)
{ return MSString(aString).center(newLength,padCharacter); }

/*------------------------------------------------------------------------------
| MSString::change                                                             |
------------------------------------------------------------------------------*/
MSString& MSString::change(const char *pPattern,unsigned patternLen,
                           const char *pReplacement,unsigned replacementLen,
                           unsigned startPos,unsigned numChanges)
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  setBuffer(_pBuffer->change(pPattern,patternLen,pReplacement,replacementLen,startPos,numChanges));
  oldBuffer->removeRef();  
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::change                                                             |
------------------------------------------------------------------------------*/
MSString MSString::change(const MSString &aString,const MSString &aPattern,
                          const MSString &aReplacement,
                          unsigned startPos,unsigned numChanges)
{
  return MSString(aString).change(aPattern,aReplacement,startPos,numChanges);
}

/*------------------------------------------------------------------------------
| MSString::copy                                                               |
------------------------------------------------------------------------------*/
MSString& MSString::copy(unsigned numCopies)
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  setBuffer(_pBuffer->copy(numCopies));
  oldBuffer->removeRef();  
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::insert                                                             |
------------------------------------------------------------------------------*/
MSString& MSString::insert(const char *pString,unsigned len,unsigned index,char padCharacter)
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  setBuffer(_pBuffer->insert(pString,len,index,padCharacter));
  oldBuffer->removeRef();  
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::leftJustify                                                        |
------------------------------------------------------------------------------*/
MSString& MSString::leftJustify(unsigned newLength,char padCharacter)
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  setBuffer(_pBuffer->leftJustify(newLength,padCharacter));
  oldBuffer->removeRef();  
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::lowerCase                                                          |
------------------------------------------------------------------------------*/
MSString& MSString::lowerCase()
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  setBuffer(_pBuffer->lowerCase());              
  oldBuffer->removeRef();  
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::overlayWith                                                        |
------------------------------------------------------------------------------*/
MSString& MSString::overlayWith(const char *pOverlay,unsigned len,unsigned index,char padCharacter)
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  setBuffer(_pBuffer->overlayWith(pOverlay,len,index,padCharacter));
  oldBuffer->removeRef();  
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::reverse                                                            |
------------------------------------------------------------------------------*/
MSString& MSString::reverse()
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  setBuffer(_pBuffer->reverse());                
  oldBuffer->removeRef();  
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::strip                                                              |
------------------------------------------------------------------------------*/
MSString& MSString::strip()
{ return strip(MSStringTest(APLUS_ISPACE),MSStringEnum::Both); }

/*------------------------------------------------------------------------------
| MSString::strip                                                              |
------------------------------------------------------------------------------*/
MSString& MSString::strip(const char *pString,unsigned len,MSStringEnum::StripMode mode)
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  setBuffer(_pBuffer->strip(pString,len,mode));
  oldBuffer->removeRef();  
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::strip                                                              |
------------------------------------------------------------------------------*/
MSString& MSString::strip(const MSStringTest &aTest,MSStringEnum::StripMode mode)
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  setBuffer(_pBuffer->strip(aTest,mode));
  oldBuffer->removeRef();  
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::translate                                                          |
------------------------------------------------------------------------------*/
MSString& MSString::translate(const char *pInputChars, unsigned inputLen,
			   const char *pOutputChars,unsigned outputLen,
			   char padCharacter)
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  setBuffer(_pBuffer->translate(pInputChars,inputLen,pOutputChars,outputLen,padCharacter));
  oldBuffer->removeRef();  
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::upperCase                                                          |
------------------------------------------------------------------------------*/
MSString& MSString::upperCase()
{
  MSStringBuffer *oldBuffer=_pBuffer;                            
  setBuffer(_pBuffer->upperCase());              
  oldBuffer->removeRef();  
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::upperCase                                                          |
------------------------------------------------------------------------------*/
MSString MSString::upperCase(const MSString &aString)
{ return MSString(aString).upperCase(); }

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the word manipulation          *
*   functions of the class MSString.                                           *
*******************************************************************************/
// Class to record occurrence of a word:
struct WordOccurrence 
{
  WordOccurrence *pNext;
  unsigned        pos;
  unsigned        len;

  WordOccurrence(unsigned p):pos(p),len(0),pNext(0) {}
  ~WordOccurrence() { if (pNext!=0) delete pNext; };
};

// Class to hold length/position for all words:
struct Words 
{
  unsigned        count;
  WordOccurrence *p;
  
  Words(const MSString& aString);
  ~Words() { delete p; }
  
  WordOccurrence &operator[](unsigned n) const;
};

/*------------------------------------------------------------------------------
| Words::operator[]                                                            |
|                                                                              |
| The wordOccurance is returned by the operator []                             |
|                                                                              |
| Notes:                                                                       |
|   1) Index is 0 based.                                                       |
|   2) If the index is greater than the number of words then return the head   |
|      of the linked list which has a pos of 0.                                |
------------------------------------------------------------------------------*/
inline WordOccurrence& Words::operator[](unsigned n) const
{
  WordOccurrence *result=p;
  if (n>=count) return *result;
  result=result->pNext;
  while (n--) result=result->pNext;
  return *result;
}

/*------------------------------------------------------------------------------
| Words::Words                                                                 |
|                                                                              |
| The constructor for the Words class parses the argument MSString for         |
| whitespace-delimited "words."  The resulting Words object contains a count   |
| of the number of words and a pointer to an array of WordOccurrence objects.  |
| Each Word object specifies the starting index of the word in the receiver    |
| and the length of the word.                                                  |
|                                                                              |
|   A word is parsed by:                                                       |
|     1. Looking for the first non-whitespace character(via the                |
|        function indexOfAnyBut()).                                            |
|     2. Recording this position as the start of the word.                     |
|     3. Scanning for the next whitespace character(via                        |
|        indexOfAnyOf()),or,the end of the argument MSString.                  |
|     4. Calculating from this the length of the word.                         |
|     5. Performing steps 1-4 until all the words have been parsed.            |
|                                                                              |
|   During this process,the position and length of each word are               |
|   recorded in a chain of WordOccurrence objects.                             |
------------------------------------------------------------------------------*/
static const char whiteSpace[]="\t\n\v\f\r ";

Words::Words(const MSString &aString):p(new WordOccurrence(0)),count(0)
{
  WordOccurrence *pLast=p;
  unsigned pos=0;
  while (pos!=aString.length())
   {
     pos=aString.indexOfAnyBut(whiteSpace,pos);
     if (pos<aString.length())
      {
	count++;
	pLast->pNext=new WordOccurrence(pos);
	pLast=pLast->pNext;
	unsigned j=aString.indexOfAnyOf(whiteSpace,pos);
	if (j==aString.length())
	 {
	   j=aString.size()-pos;
	   pos=aString.length();
	 }
	else
	 {
	   j-=pos;
	   pos+=j;
	 }
	pLast->len=j;
      }
   }
}

/*------------------------------------------------------------------------------
| MSString::removeWords                                                        |
|                                                                              |
| First,the position of the first word to be deleted is calculated via a call  |
| to indexOfWord().  If the word is found,then the position of the next word   |
| that would not be deleted is calculated(again,via indexOfWord()),and the     |
| substring deleted via a call to remove().                                    |
------------------------------------------------------------------------------*/
MSString& MSString::removeWords(unsigned firstWord,unsigned numWords)
{
  if (numWords!=0)
   {
     unsigned startIndex=indexOfWord(firstWord,0,0);
     if (startIndex<length())
      {
	// Get index of first remaining word:
	unsigned stopIndex=indexOfWord(firstWord+numWords,startIndex,firstWord);
	if (stopIndex==length()) stopIndex=length()+1;
	remove(startIndex,stopIndex-startIndex);
      }
   }
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::indexOfPhrase                                                      |
|                                                                              |
| Simply returns the character index of the phrase as determined by            |
| MSString::findPhrase().                                                      |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::findPhrase                                                         |
|                                                                              |
| This function first parses the words in both the phrase and the receiver.    |
| It then compares a word sequence from the receiver to the phrase,starting    |
| at the startWord-th word of the receiver. If all the words in the phrase are |
| matched,then the position of the current receiver word sequence is returned  |
| as the result.                                                               |
|                                                                              |
| Otherwise,the next word in the receiver is advanced to and this sequence     |
| compared to the phrase.  If all possible word sequences in the receiver are  |
| tested and fail,then 0 is returned.  All word compares are via memcmp() and  |
| are only done if the word lengths compare.                                   |
|                                                                              |
| The IndexType argument determines whether the returned result is the index   |
| within the string,or,the word index.                                         |
------------------------------------------------------------------------------*/
unsigned MSString::findPhrase(const MSString &aPhrase,unsigned startWord,IndexType charOrWord) const
{
  // Parse receiver words.
  Words stringWords(*this);
  // Parse phrase words.
  Words phraseWords(aPhrase);
  // Start at specified word.
  unsigned int stringIndex=startWord;
  // Search till we run off end of receiver(or till match found):
  while (stringIndex+phraseWords.count<=stringWords.count)
   {
     unsigned j=0; // Index into phrase.
     while (j<phraseWords.count)
      {
	if (stringWords[stringIndex+j].len!=phraseWords[j].len) break;
	if (memcmp(data()+stringWords[stringIndex+j].pos,aPhrase.data()+phraseWords[j].pos,phraseWords[j].len)!=0)
	 break;
        j++;
      }
     // Loop exit completed, phrase matched at stringIndex.
     if (j>=phraseWords.count) return ((charOrWord==CharIndex)?stringWords[stringIndex].pos:stringIndex);
     else stringIndex++; // Loop exit was due to mismatch, advance index.
   }
  // No match was found.
  return ((charOrWord==CharIndex)?length():stringWords.count);
}

/*------------------------------------------------------------------------------
| MSString::indexOfWord                                                        |
|                                                                              |
| Simply returns the result of evaluating the protected member function of the |
| same name that accepts additional arguments for the position at which to     |
| start searching and the number of words occurring prior to this position.    |
------------------------------------------------------------------------------*/
unsigned MSString::indexOfWord(unsigned wordNumber,unsigned startPos,unsigned numWords) const
{
  unsigned result=length();
  while (startPos<length()&&result==length())
   {
     // Skip to start of next word:
     startPos=indexOfAnyBut(whiteSpace,startPos);
     // See if this is the word we are looking for:
     if (startPos<length())
      {
	if (numWords==wordNumber) result=startPos;      // This is the word we are looking for:
	else startPos=indexOfAnyOf(whiteSpace,startPos);  // Skip to end of this word:
        numWords++;
      }
   }
  return result;
}

/*------------------------------------------------------------------------------
| MSString::lengthOfWord                                                       |
|                                                                              |
| First,the index of the specified word is determined via a call to            |
| indexOfWord().  Then,the receiver is scanned starting at this location for   |
| the next whiteSpace character(or the end of the receiver) via                |
| indexOfAnyOf().  The length of the word is calculated using the results of   |
| these two functions.                                                         |
------------------------------------------------------------------------------*/
unsigned MSString::lengthOfWord(unsigned wordNumber) const
{
  unsigned len=0;
  unsigned pos=indexOfWord(wordNumber,0,0);
  if (pos<length())
   {
     unsigned end=indexOfAnyOf(whiteSpace,pos+1);
     if (end<length()) len=end-pos;
     else              len=length()-pos;
   }
  return len;
}

/*------------------------------------------------------------------------------
| MSString::numWords                                                           |
|                                                                              |
| Simply parse the words in the receiver and return the resulting count.       |
------------------------------------------------------------------------------*/
unsigned MSString::numWords() const
{
  Words myWords(*this);
  return myWords.count;
}

/*------------------------------------------------------------------------------
| MSString::wordIndexOfPhrase                                                  |
|                                                                              |
| Invoke findPhrase and ask for the result as the word index.                  |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::space                                                              |
|                                                                              |
| First,the words in the receiver are parsed.  Then,space is allocated for     |
| the resulting string(with the specified number of blanks between words).     |
| Finally,the words are processed,one at a time,inserting the requisite        |
| number of blanks between them.                                               |
------------------------------------------------------------------------------*/
MSString& MSString::space(unsigned numSpaces,char spaceChar)
{
  if (length()!=0)
   {
     Words myWords(*this);
     if (myWords.count!=0)
      {
	MSStringBuffer *oldBuffer=_pBuffer;                            
	unsigned i=1; // Word index.
	unsigned j=0; // Total length(excluding first word).
	// Calculate length of spaces/words:
	while (i<myWords.count) 
	 { 
           j=MSStringBuffer::checkAddition(j,MSStringBuffer::checkAddition(numSpaces,myWords[i].len)); 
           i++;
         }
	// Construct new buffer,note first word is copied automatically:
	setBuffer(_pBuffer->newBuffer(data()+myWords[0].pos,myWords[0].len,0,j,0,0,spaceChar));
	i=1,j=0;
	while (i<myWords.count)
         {
           // 1. Add length of previous word,plus numSpaces,to j
           // 2. Copy this word from pOld->*this:
           memcpy(data()+(j+=myWords[i-1].len+numSpaces),oldBuffer->contents()+myWords[i].pos,myWords[i].len);
           i++;
         }
	oldBuffer->removeRef();
      }
     else *this=MSString::null;
   }
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::word                                                               |
|                                                                              |
| The position of the specified word is obtained via a call to indexOfWord().  |
| Then,the length of this word is deduced via a call to indexOfAnyOf()         |
|(using whiteSpace as the set of valid characters).  Finally,a result string   |
| is built from the portion of the receiver between these two positions.       |
------------------------------------------------------------------------------*/
MSString MSString::word(unsigned wordNumber) const
{
  // Treat word zero like word 1:
  unsigned start=indexOfWord(wordNumber);
  unsigned end=0;
  if (start<length()) end=indexOfAnyOf(whiteSpace,start);
  if (end>start) return MSString(data()+start,end-start);
  else return MSString::null;
}

/*------------------------------------------------------------------------------
| MSString::words                                                              |
|                                                                              |
| First,we locate the index of the first word.  If found,then we locate the    |
| end of the last word by getting its index and then searching for a           |
| whiteSpace character.  Once these indices have been calculated,we simply     |
| allocate space for the result and copy the specified portion of the receiver.|
------------------------------------------------------------------------------*/
MSString MSString::words(unsigned firstWord,unsigned numWords) const
{
  // Treat word zero like word 1:
  unsigned start=indexOfWord(firstWord);
  unsigned len=0; // Default is none.
  if (start<length())
   {
     if (numWords!=0)
      {
	// Check for overflow...
	if (numWords-1>UINT_MAX-firstWord) numWords=UINT_MAX-firstWord+1;  // Use maximum possible.
	unsigned end=indexOfWord(firstWord+numWords-1); // Get index of last word:
	// If found, get index of end of last word:
	if (end==length()) end=lastIndexOfAnyBut(whiteSpace)+1; // Word not found,use last word.
	else // Word found,skip to next blank.
	 {
	   end=indexOfAnyOf(whiteSpace,end+1);
	   if (end==length()) end=length();
	 }
	len=end-start;
      }
     return subString(start,len);
   }
  return MSString::null;
}

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This section contains the implementation of the advanced "<type1>2<type2>" *
*   MSString conversion functions.                                             *
*******************************************************************************/
/*------------------------------------------------------------------------------
| MSString::c2b                                                                |
|                                                                              |
| If the receiver is null,then the result is a null string. Otherwise,a char   |
| array 8 times as long as the receiver is allocated.  This string is filled   |
| with ones and zeros,8 from each byte of the receiver string.                 |
------------------------------------------------------------------------------*/
MSString& MSString::c2b()
{
  if (length()!=0)
   {
     MSString     old(_pBuffer);
     MSStringBuffer *oldBuffer=_pBuffer;                            
     const char *pSource=oldBuffer->contents();
     unsigned    n     =oldBuffer->length();
     
     initBuffer(0,MSStringBuffer::checkMultiplication(n,8),0,0,0,0,'0');
     
     char *pDest=data();
     while (n--)
      {
	char c=*pSource++;
	unsigned int mask=1 << 8;
	while (mask >>= 1)
	*pDest++|=((c&mask)!=0); // Mask bit on->'1'.
      }
     oldBuffer->removeRef();
   }
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::c2d                                                                |
|                                                                              |
| If the receiver is null,then it becomes '0'. Otherwise,we convert the        |
| receiver to digits by:                                                       |
|                                                                              |
|  -Starting with the first 4 bytes to an unsigned  long and then              |
|     to a MSString using the constructor that accepts an unsigned long        |
|  -"Multiplying" by 256 and adding in the next character for each             |
|      of the rest of the characters in the receiver.                          |
------------------------------------------------------------------------------*/
MSString& MSString::c2d()
{
  if (length()!=0)
   {
     MSStringBuffer *oldBuffer=_pBuffer;                            
     unsigned const char *pSource=(unsigned char*) oldBuffer->contents();
     
     // Set init to value of first n bytes(n<=4):
     unsigned int  n=(oldBuffer->length()<4)?oldBuffer->length():4;
     unsigned long init=*pSource++;
     while (--n) init=init*256+*pSource++;
     
     // Initialize receiver with MSString equivalent of this value:
     initBuffer(init);
     
     // Now handle additional bytes:
     if (oldBuffer->length()>4)
      {
	n=oldBuffer->length()-4;
	// Pad with leading zeros to streamline math:
	rightJustify(MSStringBuffer::checkAddition(length(),MSStringBuffer::checkMultiplication(n,3)),'0');
	while (n--) decimalMath(*pSource++);
	// Strip leading zeros:
	if ((n=indexOfAnyBut("0"))<length()) remove(0,n);
      }
     oldBuffer->removeRef();
   }
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::c2x                                                                |
|                                                                              |
| If the receiver is null,return a null string.  Otherwise,allocate a char     |
| array twice as big as the receiver's.  Then,we fill the array with the       |
| ascii representation of the nibbles of each byte of the receiver string.     |
------------------------------------------------------------------------------*/
static const char hexDigits[]="0123456789ABCDEF";

MSString& MSString::c2x()
{
  if (length()!=0)
   {
     MSStringBuffer *oldBuffer=_pBuffer;                            
     const unsigned char *pSource=(unsigned char *) oldBuffer->contents();
     unsigned             n=oldBuffer->length();
     
     initBuffer(0,n,0,n);
     
     char* pDest=data();
     
     while (n--)
      {
	unsigned char c=*pSource++;
	*pDest++=hexDigits[c/16];
	*pDest++=hexDigits[c%16];
      }
     oldBuffer->removeRef();
   }
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::b2c                                                                |
|                                                                              |
| If the receiver is null,do nothing.  If the receiver is not comprised of     |
| binary digits,reset the receiver to null.                                    |
|                                                                              |
| Otherwise,we process bytes of the receiver,8 at a time,adding into a         |
| character the corresponding bits.  The resulting character is placed into    |
| the new buffer.                                                              |
------------------------------------------------------------------------------*/
MSString& MSString::b2c()
{
  if (length()!=0&&isBinaryDigits())
   {
     MSStringBuffer *oldBuffer=_pBuffer;
     const char  *pSource=oldBuffer->contents();
     unsigned int newLen  =(oldBuffer->length()+7)/8;
     unsigned int bitCount=(oldBuffer->length()-1)%8+1;
     
     initBuffer(0,newLen);
     
     unsigned char *pDest=(unsigned char *)data();
     
     while (newLen--)
      {
	unsigned char c=0;
	while (bitCount--)
	 {
	   c<<=1; // Multiply previous bits by 2.
	   c+=(*pSource++=='1'); // Add: '1'->1,'0'->0
	 }
	*pDest++=c;
	bitCount=8;
      }
     oldBuffer->removeRef();
   }
  else *this=null;
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::b2d                                                                |
|                                                                              |
| Convert the receiver to "character" and then to decimal.                     |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::b2x                                                                |
|                                                                              |
| Convert the receiver to "character" and then to hexidecimal.                 |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::d2c                                                                |
|                                                                              |
| If the receiver is not decimal digits,reset to null.                         |
|                                                                              |
| Otherwise,we convert to binary by:                                           |
|                                                                              |
|   -Converting up front as many digits as can fit in a long.                  |
|   -Converting additional digits by multiplying by 10 and adding              |
|      in additional digits.                                                   |
------------------------------------------------------------------------------*/
MSString& MSString::d2c()
{
  if (isDigits())
   {
     if (length()!=0)
      {
	// Set init to value of first n-1 bytes(n==maxLong size):
	unsigned int  n=(lengthOf(maxLong)<=length())?(lengthOf(maxLong)-1):length();
	unsigned long init=subString(0,n).asInt();
	MSStringBuffer *oldBuffer=_pBuffer;
	
	// Initialize receiver with appropriate bytes from init:
	n=sizeof init;
	initBuffer(0,n);
	unsigned char *p=(unsigned char *)this->data();
	while (n--)
	 {
	   p[n]=(unsigned char)(init%256);
	   init/=256;
	 }
	
	// Now handle additional bytes:
	if (oldBuffer->length()>lengthOf(maxLong)-1)
	 {
	   unsigned const char *pSource=(unsigned char*) oldBuffer->contents()+lengthOf(maxLong)-1;
	   n=oldBuffer->length()-lengthOf(maxLong)+1;
	   // Pad with leading zeros to streamline math:
	   rightJustify(length()+n/3,0);
	   while (n--) binaryMath(*pSource++-'0');
	 }
	
	// Strip leading zeros:
	stripLeading('\0');
	// If null,reset to "0":
	if (length()==0) *this='\0';
	oldBuffer->removeRef();
      }
   }
  else *this=MSString::null;
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::d2b                                                                |
|                                                                              |
| Convert the receiver to "character" and then to binary.                      |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::d2x                                                                |
|                                                                              |
| Convert the receiver to "character" and then to hex.                         |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| x2c                                                                          |
|                                                                              |
| Function to transform hex digit to number between 0 and 15:                  |
------------------------------------------------------------------------------*/
static inline unsigned char x2c(unsigned char x)
{ return(x>'9')?(x>'F')?x-'a'+10:x-'A'+10:x-'0'; }

/*------------------------------------------------------------------------------
| MSString::x2c                                                                |
|                                                                              |
| If the receiver is null,return a null string.  If the receiver is not        |
| comprised of hex digits,return a null string.                                |
|                                                                              |
| Otherwise,allocate a new buffer one half as big.  Process the source         |
| string two bytes(nibbles) at a time,generating one character per 2           |
| hex digits.                                                                  |
------------------------------------------------------------------------------*/
MSString& MSString::x2c()
{
  if (length()>0&&isHexDigits())
   {
     MSStringBuffer *oldBuffer=_pBuffer;
     const unsigned char  *pSource=(unsigned char *) oldBuffer->contents();
     unsigned int          newLen =(oldBuffer->length()+1)/2;
     
     // Get first digit,if length is odd,presume leading '0':
     unsigned char c=(oldBuffer->length()%2)?'0':*pSource++;
     
     initBuffer(0,newLen);
     
     unsigned char *pDest=(unsigned char *) data();
     
     while (newLen--)
      {
        // Next character is 16 times first digit plus second digit:
        *pDest++=::x2c(c)*16+::x2c(*pSource++);
        // Get first digit of next character:
        c=*pSource++;
      }
     oldBuffer->removeRef();
   }
  else *this=MSString::null;
  return *this;
}

/*------------------------------------------------------------------------------
| MSString::x2b                                                                |
|                                                                              |
| Convert the receiver to "character" and then to binary digits.               |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::x2d                                                                |
|                                                                              |
| Convert the receiver to "character" and then to decimal.                     |
------------------------------------------------------------------------------*/
// See MSString.H

/*------------------------------------------------------------------------------
| MSString::decimalMath                                                        |
|                                                                              |
| We process the digits of the receiver one at a time from right to left.  We  |
| calculate the result of multiplying this digit by 256 and adding in the      |
| input "carry in" digit(which must be in the range [0,255].                   |
|                                                                              |
| The result is then divided by 10.   The remainder is used as the result      |
| digit and the quotient is propogated as the "carry" into the calculation of  |
| the next digit to the left.                                                  |
|                                                                              |
| If,after all digits are processed,the carry is non-zero,then we convert      |
| this to decimal digits and prepend it to what was calculated so far.         |
|                                                                              |
| The receiver is prefixed with sufficient leading zeros to ensure there will  |
| be sufficient room for these carry digits.                                   |
------------------------------------------------------------------------------*/
void MSString::decimalMath(unsigned char newDigit)
{
  MSStringBuffer *oldBuffer=_pBuffer;
  const unsigned char *pSource=(unsigned char *)oldBuffer->contents()+oldBuffer->length()-1;
  
  // Add enough leading zeros to handle carry out of high order digit:
  unsigned int n=indexOfAnyBut("0"); // First non-0 digit.
  if (n==length()) n=oldBuffer->length();   // All digits are zeros.
  // Never remove leading zeros.
  if (n>3) n=3;
  initBuffer(0,oldBuffer->length(),0,3-n,0,0,'0'); // At least 3 leading 0s.
  unsigned char *pDest=(unsigned char *) data()+length()-1;
  
  n=oldBuffer->length();
  unsigned int carry=newDigit;
  while (n--)
   {
     unsigned int digit=*(pSource--)-(unsigned char)'0';
     digit=256u*digit+carry;
     carry=digit/10u;
     *pDest--='0'+(unsigned char)(digit%10u);
   }
  while (carry)
   {
     *pDest--='0'+(unsigned char)(carry%10u);
     carry=carry/10u;
   }
  oldBuffer->removeRef();
}

/*------------------------------------------------------------------------------
| MSString::binaryMath                                                         |
|                                                                              |
| We process the bytes of the receiver one at a time from right to left.  We   |
| calculate the result of multiplying each byte by 10 and adding in the input  |
| "carry in" digit(which must be in the range [0,9]).                          |
|                                                                              |
| The result is then divided by 256.   The remainder is used as the result     |
| byte and the quotient is propogated as the "carry" into the calculation of   |
| the next byte to the left.                                                   |
|                                                                              |
| If,after all bytes are processed,the carry is non-zero,then this value is    |
| used to set the leading byte.  The receiver is prefixed with a zero byte to  |
| ensure there is room for this.                                               |
------------------------------------------------------------------------------*/
void MSString::binaryMath(unsigned char newDigit)
{
  MSStringBuffer *oldBuffer=_pBuffer;
  const unsigned char *pSource=(unsigned char *) oldBuffer->contents()+oldBuffer->length()-1;
  
  // Add enough leading zeros to handle carry out of high order byte:
  unsigned int n=indexOfAnyBut("\0"); // First non-0 byte.
  n=(n==0);  // Add a zero only if first digit is non-zero.
  initBuffer(0,oldBuffer->length(),0,n);
  unsigned char *pDest=(unsigned char *) data()+length()-1;
  
  n=oldBuffer->length();
  unsigned int carry=newDigit;
  while (n--)
   {
     unsigned int digit=*pSource--;
     digit=10u*digit+carry;
     carry=digit/256u;
     *pDest--=(unsigned char)(digit%256u);
   }
  
  if (carry) *pDest=carry;
  oldBuffer->removeRef();
}

//###############################################################################
// misc primitive functions - truncate,zero,exchange,upper,lower,reverse,rotate,take,drop

MSString& MSString::truncate(unsigned len)
{ return remove(length()-len); }

MSString& MSString::removeAll(void)
{
  MSStringBuffer *oldBuffer=_pBuffer;
  setBuffer(oldBuffer->newBuffer(0,0,0,0,0,0,0));
  oldBuffer->removeRef();
  return *this;
}

MSString& MSString::exchange(unsigned index1,unsigned index2)
{
  if (index1!=index2&&index1<length()&&index2<length())
   {
     prepareToChange();
     char *dp=data();
     char a=dp[index1],b=dp[index2];
     dp[index1]=b,dp[index2]=a;
   }
  return *this;
}

MSString& MSString::upper(void)
{ return upperCase(); }
MSString upper(const MSString& aString)
{ return MSString::upperCase(aString); }

MSString& MSString::lower(void)
{ return lowerCase(); }
MSString lower(const MSString& aString)
{ return MSString::lowerCase(aString); }

MSString& MSString::rotate(int count)
{
  MSStringBuffer *oldBuffer=_pBuffer;
  setBuffer(_pBuffer->rotate(count));                
  oldBuffer->removeRef();
  return *this;
}

MSString rotate(const MSString& aString,int count)
{ return MSString(aString).rotate(count); }

MSString& MSString::take(int count)
{
  MSStringBuffer *oldBuffer=_pBuffer;
  setBuffer(_pBuffer->take(count));                
  oldBuffer->removeRef();
  return *this;
}

MSString take(const MSString& aString,int count)
{ return MSString(aString).take(count); }

MSString& MSString::drop(int count)
{
  MSStringBuffer *oldBuffer=_pBuffer;                  
  setBuffer(_pBuffer->drop(count));                
  oldBuffer->removeRef();
  return *this;
}

MSString drop(const MSString& aString,int count)
{ return MSString(aString).drop(count); }

/*--------------------------- Comparison Operators ---------------------------*/
MSBoolean operator==(const MSString &string1,const MSString &string2)
{ return MSBoolean((string1._pBuffer->compare(string2.data(),string2.length())==MSStringBuffer::equal)); }
MSBoolean operator==(const MSString &string1,const char *pString2)
{ return MSBoolean((string1._pBuffer->compare(pString2,MSString::lengthOf(pString2))==MSStringBuffer::equal)); }
MSBoolean operator==(const char *pString1,const MSString &string2)
{ return MSBoolean((string2._pBuffer->compare(pString1,MSString::lengthOf(pString1))==MSStringBuffer::equal)); }

MSBoolean operator!=(const MSString &string1,const MSString &string2)
{ return MSBoolean((string1._pBuffer->compare(string2.data(),string2.length())!=MSStringBuffer::equal)); }
MSBoolean operator!=(const MSString &string1,const char *pString2)
{ return MSBoolean((string1._pBuffer->compare(pString2,MSString::lengthOf(pString2))!=MSStringBuffer::equal)); }
MSBoolean operator!=(const char *pString1,const MSString &string2)
{ return MSBoolean((string2._pBuffer->compare(pString1,MSString::lengthOf(pString1))!=MSStringBuffer::equal)); }

MSBoolean operator>(const MSString &string1,const MSString &string2)
{ return MSBoolean((string1._pBuffer->compare(string2.data(),string2.length())==MSStringBuffer::greaterThan)); }
MSBoolean operator>(const MSString &string1,const char *pString2)
{ return MSBoolean((string1._pBuffer->compare(pString2,MSString::lengthOf(pString2))==MSStringBuffer::greaterThan)); }
MSBoolean operator>(const char *pString1,const MSString &string2)
{ return MSBoolean((string2._pBuffer->compare(pString1,MSString::lengthOf(pString1))==MSStringBuffer::lessThan)); }

MSBoolean operator>=(const MSString &string1,const MSString &string2)
{ return MSBoolean((string1._pBuffer->compare(string2.data(),string2.length())==MSStringBuffer::lessThan)==MSFalse); }
MSBoolean operator>=(const MSString &string1,const char *pString2)
{ return MSBoolean((string1._pBuffer->compare(pString2,MSString::lengthOf(pString2))==MSStringBuffer::lessThan)==MSFalse); }
MSBoolean operator>=(const char *pString1,const MSString &string2)
{ return MSBoolean((string2._pBuffer->compare(pString1,MSString::lengthOf(pString1))==MSStringBuffer::greaterThan)==MSFalse); }

MSBoolean operator<(const MSString &string1,const MSString &string2)
{ return MSBoolean((string1._pBuffer->compare(string2.data(),string2.length())==MSStringBuffer::lessThan)); }
MSBoolean operator<(const MSString &string1,const char *pString2)
{ return MSBoolean((string1._pBuffer->compare(pString2,MSString::lengthOf(pString2))==MSStringBuffer::lessThan)); }
MSBoolean operator<(const char *pString1,const MSString &string2)
{ return MSBoolean((string2._pBuffer->compare(pString1,MSString::lengthOf(pString1))==MSStringBuffer::greaterThan)); }

MSBoolean operator<=(const MSString &string1,const MSString &string2)
{ return MSBoolean((string1._pBuffer->compare(string2.data(),string2.length())==MSStringBuffer::greaterThan)==MSFalse); }
MSBoolean operator<=(const MSString &string1,const char *pString2)
{ return MSBoolean((string1._pBuffer->compare(pString2,MSString::lengthOf(pString2))==MSStringBuffer::greaterThan)==MSFalse); }
MSBoolean operator<=(const char *pString1,const MSString &string2)
{ return MSBoolean((string2._pBuffer->compare(pString1,MSString::lengthOf(pString1))==MSStringBuffer::lessThan)==MSFalse); }


