#ifndef MSMBStringBufferHEADER
#define MSMBStringBufferHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


/*******************************************************************************
* FILE NAME: MSMBStringBuffer.H                                                *
*                                                                              *
* DESCRIPTION:                                                                 *
*   This file contains the declaration(s) of the class(es):                    *
*     MSMBStringBuffer-DBCS buffer class                                       *
*                                                                              *
*******************************************************************************/
#include <stdlib.h>
#include <limits.h>

#ifndef MSStringBufferHEADER
#include <MSTypes/MSStringBuffer.H>
#endif

class MSTypesExport MSMBStringBuffer : public MSStringBuffer 
{
friend MSTypesExport MSStringBuffer *createMSMBStringBuffer(void);

/*******************************************************************************
* The MSMBStringBuffer class implements the version of MSString contents that  *
* supports mixed double-byte character set (DBCS) characters.  This            *
* class ensures that multi-byte characters are processed properly.             *
*                                                                              *
* The use of this class is transparent to the user of class MSString.          *
*******************************************************************************/
public:
/*-------------------------------- Overrides -----------------------------------
| This class re-implements the following MSStringBuffer functions as public:   |
|   subString           -See MSStringBuffer::subString.                        |
|   charType            -See MSStringBuffer::charType.                         |
|   allocate            -See MSStringBuffer::allocate.                         |
|   next                -See MSStringBuffer::next.                             |
|                                                                              |
| This class re-implements the following MSString version of MSStringBuffer    | 
| functions:                                                                   |
|   isMBCS              -See MSString::isMBCS.                                 |
|   isSBCS              -See MSString::isSBCS.                                 |
|   isValidMBCS         -See MSString::isValidMBCS.                            |
|   includesMBCS        -See MSString::includesMBCS.                           |
|   includesSBCS        -See MSString::includesSBCS.                           |
|   indexOf             -See MSString::indexOf.                                |
|   indexOfAnyBut       -See MSString::indexOfAnyBut.                          |
|   indexOfAnyOf        -See MSString::indexOfAnyOf.                           |
|   lastIndexOf         -See MSString::lastIndexOf.                            |
|   lastIndexOfAnyBut   -See MSString::lastIndexOfAnyBut.                      |
|   lastIndexOfAnyOf    -See MSString::lastIndexOfAnyOf.                       |
|   center              -See MSString::center.                                 |
|   insert              -See MSString::insert.                                 |
|   leftJustify         -See MSString::leftJustify.                            | 
|   lowerCase           -See MSString::lowerCase.                              |
|   overlayWith         -See MSString::overlayWith.                            |
|   remove              -See MSString::remove.                                 |
|   reverse             -See MSString::reverse.                                |
|   rightJustify        -See MSString::rightJustify.                           |
|   strip               -See MSString::strip.                                  |
|   translate           -See MSString::translate.                              |
|   upperCase           -See MSString::upperCase.                              |
|   className           -Returns "MSMBStringBuffer".                           |
------------------------------------------------------------------------------*/
  virtual MSStringBuffer *subString(unsigned startPos,unsigned len,char padCharacter) const;
  virtual MSStringEnum::CharType charType(unsigned index) const;
  virtual char *next(const char *prev);
  virtual const char *next(const char *prev) const;

  virtual MSBoolean isMBCS() const;
  virtual MSBoolean isSBCS() const;
  virtual MSBoolean isValidMBCS() const;
  virtual MSBoolean includesMBCS() const;
  virtual MSBoolean includesSBCS() const;

  virtual unsigned indexOf(const char *pString,unsigned len,unsigned startPos=0) const;
  virtual unsigned indexOf(const MSStringTest &aTest,unsigned startPos=0) const;
  virtual unsigned indexOfAnyBut(const char *pString,unsigned len,unsigned startPos=0) const;
  virtual unsigned indexOfAnyBut(const MSStringTest &aTest,unsigned startPos=0) const;
  virtual unsigned indexOfAnyOf(const char *pString,unsigned len,unsigned startPos=0) const;
  virtual unsigned indexOfAnyOf(const MSStringTest &aTest,unsigned startPos=0) const;

  virtual unsigned lastIndexOf(const char *pString,unsigned len,unsigned startPos=UINT_MAX-1) const;
  virtual unsigned lastIndexOf(const MSStringTest &aTest,unsigned startPos=UINT_MAX-1) const;
  virtual unsigned lastIndexOfAnyBut(const char *pString,unsigned len,unsigned startPos=UINT_MAX-1) const;
  virtual unsigned lastIndexOfAnyBut(const MSStringTest &aTest,unsigned startPos=UINT_MAX-1) const;
  virtual unsigned lastIndexOfAnyOf(const char *pString,unsigned len,unsigned startPos=UINT_MAX-1) const;
  virtual unsigned lastIndexOfAnyOf(const MSStringTest &aTest,unsigned startPos=UINT_MAX-1) const;

  virtual MSStringBuffer *center(unsigned newLen,char padCharacter);
  virtual MSStringBuffer *insert(const char *pInsert,unsigned insertLen,unsigned pos,char padCharacter);
  virtual MSStringBuffer *leftJustify(unsigned newLen,char padCharacter);
  virtual MSStringBuffer *lowerCase();
  virtual MSStringBuffer *overlayWith(const char *overlay,unsigned len,unsigned pos,char padCharacter);
  virtual MSStringBuffer *remove(unsigned startPos,unsigned numChars);
  virtual MSStringBuffer *reverse();
  virtual MSStringBuffer *rightJustify(unsigned newLen,char padCharacter);
  virtual MSStringBuffer *strip(const char *pChars,unsigned len,MSStringEnum::StripMode mode);
  virtual MSStringBuffer *strip(const MSStringTest &aTest,MSStringEnum::StripMode mode);
  virtual MSStringBuffer *translate(const char *pInputChars,unsigned inputLen,
                                    const char *pOutputChars,unsigned outputLen,char padCharacter);
  virtual MSStringBuffer *upperCase();

protected:
/*---------------------- Protected Overrides -----------------------------------
| This class re-implements the following MSStringBuffer functions as protected:|
|                                                                              |
|   startSearch         -See MSStringBuffer::startSearch.                      |
|   startBackwardsSearch-See MSStringBuffer::startBackwardsSearch.             |
|   className           -Returns "MSMBStringBuffer".                           |
------------------------------------------------------------------------------*/

  virtual MSStringBuffer *allocate(unsigned newLen) const;
  virtual const char *className() const;

  virtual unsigned startSearch(unsigned startPos,unsigned searchLen) const;
  virtual unsigned startBackwardsSearch(unsigned startPos,unsigned searchLen) const;

/*------------------------------ Implementation --------------------------------
| These functions provide services used to implement this class:               |
|   isCharValid-Returns MSTrue if and only if the character at the given       |
|                 index is in the set of valid characters.                     |
|   isDBCS1    -Returns MSTrue if and only if the byte at the given offset     |
|                 is the first byte of DBCS.                                   |
|   isPrevDBCS -Returns MSTrue if and only if the preceding character to the   |
|                 one at the given offset is a DBCS character.                 |
|   charLength -                                                               |
|   maxCharLength-returns maximum length of a multibyte character              |
|   prevCharLength-                                                            |
|   isSBC      -returns MSTrue if character is NOT a multibyte character       |
|                                                                              |
------------------------------------------------------------------------------*/
  MSBoolean isCharValid(unsigned pos,const char *pValidChars,unsigned numValidChars) const;

  static size_t charLength(char const*);
  static size_t maxCharLength();

  size_t charLength(unsigned pos) const;
  size_t prevCharLength(unsigned pos) const;

  static MSBoolean isSBC(char const*);

friend class MSStringBuffer;
/*------------------------------- Constructor ----------------------------------
| The constructor for this class is protected.  It is only called from the     |
| member function allocate and the static member function                      |
| MSStringBuffer::initialize.                                                  |
------------------------------------------------------------------------------*/
  MSMBStringBuffer(unsigned bufLength);
  ~MSMBStringBuffer(void);
};

inline size_t MSMBStringBuffer::charLength(char const *pString_)
{
  if (*pString_==0) return 1;
  else return mblen(pString_,MB_LEN_MAX);
}

inline size_t MSMBStringBuffer::charLength(unsigned pos) const
{ return charLength(contents()+pos-1); }

inline size_t MSMBStringBuffer::maxCharLength()
{ return MB_CUR_MAX; }

inline MSBoolean MSMBStringBuffer::isSBC(char const *pString_)
{ return MSBoolean(charLength(pString_)==1); }

#endif 


