#ifndef MSStringINLINES
#define MSStringINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


extern "C"
{
#include <ctype.h>
#include <string.h>
}

#ifdef __cfront
#undef isspace
#endif

#ifndef MSStringBufferHEADER
#include <MSTypes/MSStringBuffer.H>
#endif 

#ifndef MSStringTestHEADER
#include <MSTypes/MSStringTest.H>
#endif 

#ifndef MS_NO_INLINES
#define INLINELINKAGE inline
#else
#define INLINELINKAGE
#endif

/*---------------------------- INLINELINKAGE Methods --------------------------------*/
INLINELINKAGE MSStringBuffer *MSString::buffer(void) const
{
#ifdef IC_DEVELOP
  IASSERT(_pBuffer!=0);
#endif
  return _pBuffer;
}

INLINELINKAGE MSStringBuffer *MSString::defaultBuffer(void)
{ return MSStringBuffer::defaultBuffer(); }
INLINELINKAGE char *MSString::data(void) const
{ return _pBuffer->data; }
INLINELINKAGE const char *MSString::string(void) const
{ return _pBuffer->data; }

INLINELINKAGE unsigned MSString::lengthOf(const char *pString_)
{ return (pString_!=0)?strlen(pString_):0; }

INLINELINKAGE MSString &MSString::setBuffer(MSStringBuffer *pBuffer_)
{
#ifdef IC_DEVELOP
  IASSERT(pBuffer_!=0);
#endif
  _pBuffer=pBuffer_;
  return changed(),*this;
}

/*-------------------------------- Accessors ---------------------------------*/
INLINELINKAGE unsigned MSString::length(void) const
{ return _pBuffer->len; }
INLINELINKAGE unsigned MSString::size(void) const
{ return _pBuffer->len; }
INLINELINKAGE MSStringEnum::CharType MSString::charType(unsigned index) const
{ return _pBuffer->charType(index); }

INLINELINKAGE MSBoolean MSString::isSet(void) const 
{ return MSBoolean(length()>0); }

INLINELINKAGE char MSString::first(void) const 
{ return (length()>0)?((*this)(0)):0; }
INLINELINKAGE char MSString::last(void) const
{ return (length()>0)?((*this)(length()-1)):0; }

INLINELINKAGE const MSSymbol& MSString::stringType(void)
{ return symbol(); }

/*-------------------------------- Assignment ---------------------------------*/
INLINELINKAGE MSString& MSString::operator=(const MSString &aString)
{
  aString._pBuffer->addRef();                                
  _pBuffer->removeRef();
  setBuffer(aString._pBuffer);
  return *this;
}

/*-------------------------------- Searching ---------------------------------*/
INLINELINKAGE unsigned MSString::indexOf(const MSString &aString,unsigned startPos) const
{ return _pBuffer->indexOf(aString.data(),aString.length(),startPos); }
INLINELINKAGE unsigned MSString::indexOf(const char *pString,unsigned startPos) const
{ return _pBuffer->indexOf(pString,lengthOf(pString),startPos); }
INLINELINKAGE unsigned MSString::indexOf(const MSStringTest &aTest,unsigned startPos) const
{ return _pBuffer->indexOf(aTest,startPos); }
INLINELINKAGE unsigned MSString::indexOfAnyBut(const MSString &aString,unsigned startPos) const
{ return _pBuffer->indexOfAnyBut(aString.data(),aString.length(),startPos); }
INLINELINKAGE unsigned MSString::indexOfAnyBut(const char *pValidChars,unsigned startPos) const
{ return _pBuffer->indexOfAnyBut(pValidChars,lengthOf(pValidChars),startPos); }
INLINELINKAGE unsigned MSString::indexOfAnyBut(char validChar,unsigned startPos) const
{ return _pBuffer->indexOfAnyBut(&validChar,1,startPos); }
INLINELINKAGE unsigned MSString::indexOfAnyBut(const MSStringTest &aTest,unsigned startPos) const
{ return _pBuffer->indexOfAnyBut(aTest,startPos); }
INLINELINKAGE unsigned MSString::indexOfAnyOf(const MSString &aString,unsigned startPos) const
{ return _pBuffer->indexOfAnyOf(aString.data(),aString.length(),startPos); }
INLINELINKAGE unsigned MSString::indexOfAnyOf(const char *pSearchChars,unsigned startPos) const
{ return _pBuffer->indexOfAnyOf(pSearchChars,lengthOf(pSearchChars),startPos); }
INLINELINKAGE unsigned MSString::indexOfAnyOf(char searchChar,unsigned startPos) const
{ return _pBuffer->indexOfAnyOf(&searchChar,1,startPos); }
INLINELINKAGE unsigned MSString::indexOfAnyOf(const MSStringTest &aTest,unsigned startPos) const
{ return _pBuffer->indexOfAnyOf(aTest,startPos); }
INLINELINKAGE unsigned MSString::lastIndexOf(const MSString &aString,unsigned startPos) const
{ return _pBuffer->lastIndexOf(aString.data(),aString.length(),startPos); }
INLINELINKAGE unsigned MSString::lastIndexOf(const char *pString,unsigned startPos) const
{ return _pBuffer->lastIndexOf(pString,lengthOf(pString),startPos); }
INLINELINKAGE unsigned MSString::lastIndexOf(const MSStringTest &aTest,unsigned startPos) const
{ return _pBuffer->lastIndexOf(aTest,startPos); }
INLINELINKAGE unsigned MSString::lastIndexOfAnyBut(const MSString &aString,unsigned startPos) const
{ return _pBuffer->lastIndexOfAnyBut(aString.data(),aString.length(),startPos); }
INLINELINKAGE unsigned MSString::lastIndexOfAnyBut(const char *pValidChars,unsigned startPos) const
{ return _pBuffer->lastIndexOfAnyBut(pValidChars,lengthOf(pValidChars),startPos); }
INLINELINKAGE unsigned MSString::lastIndexOfAnyBut(char validChar,unsigned startPos) const
{ return _pBuffer->lastIndexOfAnyBut(&validChar,1,startPos); }
INLINELINKAGE unsigned MSString::lastIndexOfAnyBut(const MSStringTest &aTest,unsigned startPos) const
{ return _pBuffer->lastIndexOfAnyBut(aTest,startPos); }
INLINELINKAGE unsigned MSString::lastIndexOfAnyOf(const MSString &aString,unsigned startPos) const
{ return _pBuffer->lastIndexOfAnyOf(aString.data(),aString.length(),startPos); }
INLINELINKAGE unsigned MSString::lastIndexOfAnyOf(const char *pSearchChars,unsigned startPos) const
{ return _pBuffer->lastIndexOfAnyOf(pSearchChars,lengthOf(pSearchChars),startPos); }
INLINELINKAGE unsigned MSString::lastIndexOfAnyOf(char searchChar,unsigned startPos) const
{ return _pBuffer->lastIndexOfAnyOf(&searchChar,1,startPos); }
INLINELINKAGE unsigned MSString::lastIndexOfAnyOf(const MSStringTest &aTest,unsigned startPos) const
{ return _pBuffer->lastIndexOfAnyOf(aTest,startPos); }
INLINELINKAGE unsigned MSString::occurrencesOf(const MSString &aString,unsigned startPos) const
{ return occurrencesOf(aString.data(),aString.length(),startPos); }
INLINELINKAGE unsigned MSString::occurrencesOf(const char *pString,unsigned startPos) const
{ return occurrencesOf(pString,lengthOf(pString),startPos); }

/*--------------------------------- Testing ----------------------------------*/
INLINELINKAGE MSBoolean MSString::isAlphanumeric() const
{ return _pBuffer->isAlphanumeric(); }
INLINELINKAGE MSBoolean MSString::isAlphabetic() const
{ return _pBuffer->isAlphabetic(); }
INLINELINKAGE MSBoolean MSString::isASCII() const
{ return _pBuffer->isASCII(); }
INLINELINKAGE MSBoolean MSString::isControl() const
{ return _pBuffer->isControl(); }
INLINELINKAGE MSBoolean MSString::isDigits() const
{ return _pBuffer->isDigits(); } 
INLINELINKAGE MSBoolean MSString::isGraphics() const
{ return _pBuffer->isGraphics(); } 
INLINELINKAGE MSBoolean MSString::isHexDigits() const
{ return _pBuffer->isHexDigits(); } 
INLINELINKAGE MSBoolean MSString::isLowerCase() const
{ return _pBuffer->isLowerCase(); } 
INLINELINKAGE MSBoolean MSString::isPrintable() const
{ return _pBuffer->isPrintable(); } 
INLINELINKAGE MSBoolean MSString::isPunctuation() const
{ return _pBuffer->isPunctuation(); } 
INLINELINKAGE MSBoolean MSString::isUpperCase() const
{ return _pBuffer->isUpperCase(); } 
INLINELINKAGE MSBoolean MSString::isWhiteSpace() const
{ return _pBuffer->isWhiteSpace(); } 
INLINELINKAGE MSBoolean MSString::isMBCS() const
{ return _pBuffer->isMBCS(); }
INLINELINKAGE MSBoolean MSString::isDBCS() const
{ return _pBuffer->isDBCS(); }
INLINELINKAGE MSBoolean MSString::isSBCS() const
{ return _pBuffer->isSBCS(); }
INLINELINKAGE MSBoolean MSString::isValidMBCS() const
{ return _pBuffer->isValidMBCS(); }
INLINELINKAGE MSBoolean MSString::isValidDBCS() const
{ return _pBuffer->isValidDBCS(); }
INLINELINKAGE MSBoolean MSString::includesMBCS() const
{ return _pBuffer->includesMBCS(); }
INLINELINKAGE MSBoolean MSString::includesDBCS() const
{ return _pBuffer->includesDBCS(); }
INLINELINKAGE MSBoolean MSString::includesSBCS() const
{ return _pBuffer->includesSBCS(); }
INLINELINKAGE MSBoolean MSString::isLike(const MSString &aPattern,char zeroOrMore,char anyChar) const
{ return isLike(aPattern.data(),aPattern.length(),zeroOrMore,anyChar); }
INLINELINKAGE MSBoolean MSString::isLike(const char *pPattern,char zeroOrMore,char anyChar) const
{ return isLike(pPattern,lengthOf(pPattern),zeroOrMore,anyChar); }
INLINELINKAGE MSBoolean MSString::isAbbreviationFor(const MSString &aString,unsigned minAbbrevLength) const
{ return isAbbrevFor(aString.data(),aString.length(),minAbbrevLength); }
INLINELINKAGE MSBoolean MSString::isAbbreviationFor(const char *pString,unsigned minAbbrevLength) const
{ return isAbbrevFor(pString,lengthOf(pString),minAbbrevLength); }

INLINELINKAGE MSBoolean MSString::includes(const MSString &aString) const
{ return MSBoolean(indexOf(aString)<length()); }
INLINELINKAGE MSBoolean MSString::includes(const char *pString) const
{ return MSBoolean(indexOf(pString)<length()); }
INLINELINKAGE MSBoolean MSString::includes(char aChar) const
{ return MSBoolean(indexOf(aChar)<length()); }
INLINELINKAGE MSBoolean MSString::includes(const MSStringTest &aTest) const
{ return MSBoolean(indexOf(aTest)<length()); }

/*--------------------------- Comparison Operators ---------------------------*/
INLINELINKAGE MSBoolean MSString::operator!() const
{ return MSBoolean(length()==0); } 

INLINELINKAGE long MSString::compare(const MSString &aString_) const
{
  switch (_pBuffer->compare(aString_.data(),aString_.length()))
   {
   case MSStringBuffer::equal:       return 0;
   case MSStringBuffer::greaterThan: return 1;
   case MSStringBuffer::lessThan:    return -1;
   }
  return 0;
}

/*-------------------------------- Conversion --------------------------------*/
INLINELINKAGE MSString MSString::asString() const
{ return MSString(*this); }
INLINELINKAGE long int MSString::asInt() const
{ return strtol(data(),0,10); }
INLINELINKAGE unsigned long MSString::asUnsigned() const
{ return strtoul(data(),0,10); }
INLINELINKAGE double MSString::asDouble() const
{ return strtod(data(),0); }
INLINELINKAGE MSString::operator const char*() const
{ return data(); }
INLINELINKAGE MSString::operator const unsigned char*() const
{ return (unsigned char*)data(); }

INLINELINKAGE MSString MSString::c2b(const MSString &aString)
{ return MSString(aString).c2b(); }
INLINELINKAGE MSString MSString::c2d(const MSString &aString)
{ return MSString(aString).c2d(); }
INLINELINKAGE MSString MSString::c2x(const MSString &aString)
{ return MSString(aString).c2x(); }
INLINELINKAGE MSString MSString::b2c(const MSString &aString)
{ return MSString(aString).b2c(); }
INLINELINKAGE MSString& MSString::b2d()
{ return b2c().c2d(); }
INLINELINKAGE MSString MSString::b2d(const MSString &aString)
{ return MSString(aString).b2d(); }
INLINELINKAGE MSString& MSString::b2x()
{ return b2c().c2x(); }
INLINELINKAGE MSString MSString::b2x(const MSString &aString)
{ return MSString(aString).b2x(); }
INLINELINKAGE MSString MSString::d2c(const MSString &aString)
{ return MSString(aString).d2c(); }
INLINELINKAGE MSString& MSString::d2b()
{ return d2c().c2b(); }
INLINELINKAGE MSString MSString::d2b(const MSString &aString)
{ return MSString(aString).d2b(); }
INLINELINKAGE MSString& MSString::d2x()
{ return d2c().c2x(); }
INLINELINKAGE MSString MSString::d2x(const MSString &aString)
{ return MSString(aString).d2x(); }
INLINELINKAGE MSString MSString::x2c(const MSString &aString)
{ return MSString(aString).x2c(); }
INLINELINKAGE MSString& MSString::x2b()
{ return x2c().c2b(); }
INLINELINKAGE MSString MSString::x2b(const MSString &aString)
{ return MSString(aString).x2b(); }
INLINELINKAGE MSString& MSString::x2d()
{ return x2c().c2d(); }
INLINELINKAGE MSString MSString::x2d(const MSString &aString)
{ return MSString(aString).x2d(); }

/*--------------------------------- Editing ----------------------------------*/
INLINELINKAGE MSString& MSString::change(const MSString &aPattern,const MSString &aReplacement,
                                unsigned startPos,unsigned numChanges)
{ return change(aPattern.data(),aPattern.length(),aReplacement.data(),aReplacement.length(),startPos,numChanges); }
INLINELINKAGE MSString& MSString::change(const MSString &aPattern,const char *pReplacement,unsigned startPos,unsigned numChanges)
{ return change(aPattern.data(),aPattern.length(),pReplacement,lengthOf(pReplacement),startPos,numChanges); }
INLINELINKAGE MSString& MSString::change(const char *pPattern,const MSString &aReplacement,unsigned startPos,unsigned numChanges)
{ return change(pPattern,lengthOf(pPattern),aReplacement.data(),aReplacement.length(),startPos,numChanges); }
INLINELINKAGE MSString& MSString::change(const char *pPattern,const char *pReplacement,unsigned startPos,unsigned numChanges)
{ return change(pPattern,lengthOf(pPattern),pReplacement,lengthOf(pReplacement),startPos,numChanges); }
INLINELINKAGE MSString MSString::change(const MSString &aString,const MSString &aPattern,const char *pReplacement,
			       unsigned startPos,unsigned numChanges)
{ return MSString(aString).change(aPattern,pReplacement,startPos,numChanges); }
INLINELINKAGE MSString MSString::change(const MSString &aString,const char *pPattern,const MSString &aReplacement,
			       unsigned startPos,unsigned numChanges)
{ return MSString(aString).change(pPattern,aReplacement,startPos,numChanges); }
INLINELINKAGE MSString MSString::change(const MSString &aString,const char *pPattern,const char *pReplacement,
			       unsigned startPos,unsigned numChanges)
{ return MSString(aString).change(pPattern,pReplacement,startPos,numChanges); }
INLINELINKAGE MSString MSString::copy(const MSString &aString,unsigned numCopies)
{ return MSString(aString).copy(numCopies); }
INLINELINKAGE MSString& MSString::insert(const MSString &aString,unsigned index,char padCharacter)
{ return insert(aString.data(),aString.length(),index,padCharacter); }
INLINELINKAGE MSString& MSString::insert(const char *pString,unsigned index,char padCharacter)
{ return insert(pString,lengthOf(pString),index,padCharacter); }
INLINELINKAGE MSString MSString::insert(const MSString &aString,const MSString &anInsert,unsigned index,char padCharacter)
{ return MSString(aString).insert(anInsert,index,padCharacter); }
INLINELINKAGE MSString MSString::insert(const MSString &aString,const char *pInsert,unsigned index,char padCharacter)
{ return MSString(aString).insert(pInsert,index,padCharacter); }
INLINELINKAGE MSString MSString::leftJustify(const MSString &aString,unsigned newLength,char padCharacter)
{ return MSString(aString).leftJustify(newLength,padCharacter); }
INLINELINKAGE MSString MSString::lowerCase(const MSString &aString)
{ return MSString(aString).lowerCase(); }
INLINELINKAGE MSString& MSString::overlayWith(const MSString &aString,unsigned index,char padCharacter)
{ return overlayWith(aString.data(),aString.length(),index,padCharacter); }
INLINELINKAGE MSString& MSString::overlayWith(const char *pString,unsigned index,char padCharacter)
{ return overlayWith(pString,lengthOf(pString),index,padCharacter); }
INLINELINKAGE MSString MSString::overlayWith(const MSString &aString,const MSString &anOverlay,unsigned index,char padCharacter)
{ return MSString(aString).overlayWith(anOverlay,index,padCharacter); }
INLINELINKAGE MSString MSString::overlayWith(const MSString &aString,const char *pOverlay,unsigned index,char padCharacter)
{ return MSString(aString).overlayWith(pOverlay,index,padCharacter); }
INLINELINKAGE MSString MSString::remove(const MSString &aString,unsigned startPos)
{ return MSString::remove(aString,startPos,UINT_MAX); }
INLINELINKAGE MSString MSString::reverse(const MSString &aString)
{ return MSString(aString).reverse(); }
INLINELINKAGE MSString MSString::rightJustify(const MSString &aString,unsigned newLength,char padCharacter)
{ return MSString(aString).rightJustify(newLength,padCharacter); }
INLINELINKAGE MSString& MSString::stripLeading()
{ return strip(MSStringTest(APLUS_ISPACE),MSStringEnum::Leading); }
INLINELINKAGE MSString& MSString::stripTrailing()
{ return strip(MSStringTest(APLUS_ISPACE),MSStringEnum::Trailing); }
INLINELINKAGE MSString& MSString::strip(char aChar)
{ return strip(&aChar,1,MSStringEnum::Both); }
INLINELINKAGE MSString& MSString::stripLeading(char aChar)
{ return strip(&aChar,1,MSStringEnum::Leading); }
INLINELINKAGE MSString& MSString::stripTrailing(char aChar)
{ return strip(&aChar,1,MSStringEnum::Trailing); }
INLINELINKAGE MSString& MSString::strip(const MSString &aString)
{ return strip(aString.data(),aString.length(),MSStringEnum::Both); }
INLINELINKAGE MSString& MSString::stripLeading(const MSString &aString)
{ return strip(aString.data(),aString.length(),MSStringEnum::Leading); }
INLINELINKAGE MSString& MSString::stripTrailing(const MSString &aString)
{ return strip(aString.data(),aString.length(),MSStringEnum::Trailing); }
INLINELINKAGE MSString& MSString::strip(const char *pString)
{ return strip(pString,lengthOf(pString),MSStringEnum::Both); }
INLINELINKAGE MSString& MSString::stripLeading(const char *pString)
{ return strip(pString,lengthOf(pString),MSStringEnum::Leading); }
INLINELINKAGE MSString& MSString::stripTrailing(const char *pString)
{ return strip(pString,lengthOf(pString),MSStringEnum::Trailing); }
INLINELINKAGE MSString& MSString::strip(const MSStringTest &aTest)
{ return strip(aTest,MSStringEnum::Both); }
INLINELINKAGE MSString& MSString::stripLeading(const MSStringTest &aTest)
{ return strip(aTest,MSStringEnum::Leading); }
INLINELINKAGE MSString& MSString::stripTrailing(const MSStringTest &aTest)
{ return strip(aTest,MSStringEnum::Trailing); }
INLINELINKAGE MSString MSString::stripBlanks(const MSString &aString)
{ return MSString(aString).strip(); }
INLINELINKAGE MSString MSString::strip(const MSString &aString,char aChar)
{ return MSString(aString).strip(aChar); }
INLINELINKAGE MSString MSString::strip(const MSString &aString,const MSString &aStringOfChars)
{ return MSString(aString).strip(aStringOfChars); }
INLINELINKAGE MSString MSString::strip(const MSString &aString,const char *pStringOfChars)
{ return MSString(aString).strip(pStringOfChars); }
INLINELINKAGE MSString MSString::strip(const MSString &aString,const MSStringTest &aTest)
{ return MSString(aString).strip(aTest); }
INLINELINKAGE MSString MSString::stripLeadingBlanks(const MSString &aString)
{ return MSString(aString).stripLeading(); }
INLINELINKAGE MSString MSString::stripLeading(const MSString &aString,char aChar)
{ return MSString(aString).stripLeading(aChar); }
INLINELINKAGE MSString MSString::stripLeading(const MSString &aString,const MSString &aStringOfChars)
{ return MSString(aString).stripLeading(aStringOfChars); }
INLINELINKAGE MSString MSString::stripLeading(const MSString &aString,const char *pStringOfChars)
{ return MSString(aString).stripLeading(pStringOfChars); }
INLINELINKAGE MSString MSString::stripLeading(const MSString &aString,const MSStringTest &aTest)
{ return MSString(aString).stripLeading(aTest); }
INLINELINKAGE MSString MSString::stripTrailingBlanks(const MSString &aString)
{ return MSString(aString).stripTrailing(); }
INLINELINKAGE MSString MSString::stripTrailing(const MSString &aString,char aChar)
{ return MSString(aString).stripTrailing(aChar); }
INLINELINKAGE MSString MSString::stripTrailing(const MSString &aString,const MSString &aStringOfChars)
{ return MSString(aString).stripTrailing(aStringOfChars); }
INLINELINKAGE MSString MSString::stripTrailing(const MSString &aString,const char *pStringOfChars)
{ return MSString(aString).stripTrailing(pStringOfChars); }
INLINELINKAGE MSString MSString::stripTrailing(const MSString &aString,const MSStringTest &aTest)
{ return MSString(aString).stripTrailing(aTest); }
INLINELINKAGE MSString& MSString::translate(const MSString &inputChars,const MSString &outputChars,char padCharacter)
{ return translate(inputChars.data(),inputChars.length(),outputChars.data(),outputChars.length(),padCharacter); }
INLINELINKAGE MSString& MSString::translate(const MSString &inputChars,const char *pOutputChars,char padCharacter)
{ return translate(inputChars.data(),inputChars.length(),pOutputChars,lengthOf(pOutputChars),padCharacter); }
INLINELINKAGE MSString& MSString::translate(const char *pInputChars,const MSString &outputChars,char padCharacter)
{ return translate(pInputChars,lengthOf(pInputChars),outputChars.data(),outputChars.length(),padCharacter); }
INLINELINKAGE MSString& MSString::translate(const char *pInputChars,const char *pOutputChars,char padCharacter)
{ return translate(pInputChars,lengthOf(pInputChars),pOutputChars,lengthOf(pOutputChars),padCharacter); }
INLINELINKAGE MSString MSString::translate(const MSString &aString,const MSString &inputChars,
				 const MSString &outputChars,char padCharacter)
{ return MSString(aString).translate(inputChars,outputChars,padCharacter); }
INLINELINKAGE MSString MSString::translate(const MSString &aString,const MSString &inputChars,
				 const char *pOutputChars,char padCharacter)
{ return MSString(aString).translate(inputChars,pOutputChars,padCharacter); }
INLINELINKAGE MSString MSString::translate(const MSString &aString,const char *pInputChars,
				 const MSString &outputChars,char padCharacter)
{ return MSString(aString).translate(pInputChars,outputChars,padCharacter); }
INLINELINKAGE MSString MSString::translate(const MSString &aString,const char *pInputChars,
				 const char *pOutputChars,char padCharacter)
{ return MSString(aString).translate(pInputChars,pOutputChars,padCharacter); }

/*------------------------------ Word Functions ------------------------------*/
INLINELINKAGE MSString& MSString::removeWords(unsigned firstWord)
{ return removeWords(firstWord,UINT_MAX); }
INLINELINKAGE MSString MSString::removeWords(const MSString &aString,unsigned firstWord)
{ return MSString(aString).removeWords(firstWord); }
INLINELINKAGE MSString MSString::removeWords(const MSString &aString,unsigned firstWord,unsigned numWords)
{ return MSString(aString).removeWords(firstWord,numWords); }
INLINELINKAGE unsigned MSString::indexOfPhrase(const MSString &aPhrase,unsigned startWord) const
{ return findPhrase(aPhrase,startWord,CharIndex); }
INLINELINKAGE unsigned MSString::indexOfWord(unsigned wordNumber) const
{ return indexOfWord(wordNumber,0,0); }
INLINELINKAGE unsigned MSString::wordIndexOfPhrase(const MSString &aPhrase,unsigned startWord) const
{ return findPhrase(aPhrase,startWord,WordIndex); }
INLINELINKAGE MSString MSString::space(const MSString &aString,unsigned numSpaces,char spaceChar)
{ return MSString(aString).space(numSpaces,spaceChar); }
INLINELINKAGE MSString MSString::words(unsigned firstWord) const
{ return words(firstWord,UINT_MAX); }

/*------------------------------ Subscripting operators ---------------------------*/
INLINELINKAGE MSString::CharPick MSString::operator[](int index)
{ return (*this)[(unsigned)index]; }

INLINELINKAGE const char &MSString::operator[](int index) const
{ return (*this)[(unsigned)index]; }

/*------------------------------ CharPick Methods ------------------------------*/
INLINELINKAGE MSString::CharPick::CharPick(MSString *pString,unsigned index) 
{ _pString=pString,_index=index; }
INLINELINKAGE MSString::CharPick::CharPick(const CharPick& rChar)   
{ _pString=rChar._pString,_index=rChar._index; }

INLINELINKAGE MSString *MSString::CharPick::string() const 
{ return _pString; }
INLINELINKAGE char MSString::CharPick::value() const 
{ return (*_pString)(_index); }
INLINELINKAGE unsigned MSString::CharPick::index() const 
{ return _index; }

INLINELINKAGE MSString::CharPick& MSString::CharPick::operator=(char aChar) 
{ _pString->set(_index,aChar); return *this; }
INLINELINKAGE MSString::CharPick::operator char() const 
{ return value(); } 

#endif 
