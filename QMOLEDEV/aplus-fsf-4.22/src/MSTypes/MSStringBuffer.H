#ifndef MSStringBufferHEADER
#define MSStringBufferHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This file contains the declaration(s) of the class(es):                    *
*     MSStringBuffer - class to hold MSString contents                         *
*******************************************************************************/
extern "C"
{
#include <limits.h>
}

#include <MSTypes/MSEnum.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#ifdef MS_MULTI_THREAD
#include <MSTypes/MSMutex.H>
#endif

#ifndef MSStringEnumHEADER
#include <MSTypes/MSStringEnum.H>
#endif

class MSStringTest;
class MSString;

// MSBaseStringBuffer class is needed here so that that virtual function table
// pointer gets placed on top of MSStringBuffer in memory layout, in the base
// class portion, rather than after the MSStringBuffer data members, since we
// are going to allocate more space than we declare to the compiler.
//
class MSTypesExport MSBaseStringBuffer
{
public:
  virtual ~MSBaseStringBuffer();
};

class MSTypesExport MSStringBuffer : public MSBaseStringBuffer
{
friend class MSString;
/*******************************************************************************
* The MSStringBuffer class defines the contents of an MSString.                *
*                                                                              *
*   Notes:                                                                     *
*                                                                              *
*     1) This class is intended only to support the implementation of the      *
*        MSString class.  Objects of this class should not be created or       *
*        manipulated except through the containing MSString object.            *
*                                                                              *
*     2) Use the following information to guide you when allocating            *
*        objects of this class:                                                *
*                                                                              *
*        - Subclasses must not add any data members.                           *
*                                                                              *
*        - The operator new is protected so that it can be called only from    *
*          the newBuffer member function.                                      *
*                                                                              *
*        - The destructor is protected so that instances of this class cannot  *
*          easily be created on the stack or as statics.                       *
*******************************************************************************/
public:
/*------------------------------- Reallocating ---------------------------------
| The following function manages the reallocation of MSStringBuffers when      |
| strings' contents are modified:                                              |
|   newBuffer           - Allocates a new buffer and initializes it with the   |
|                         contents of up to three argument buffers.            |
|   checkAddition       - Verifies that the two arguments, when added, will    |
|                         not overflow an unsigned int.                        |
|   checkMultiplication - Verifies that the two arguments, when multiplied,    |
|                         will not overflow an unsigned int.                   |
|   overflow            - Function that throws exception when overflow is      |
|                         detected by checkAddition or checkMultiplication.    |
------------------------------------------------------------------------------*/
  MSStringBuffer *newBuffer(const void *p1,unsigned len1,
                            const void *p2=0,unsigned len2=0,
                            const void *p3=0,unsigned len3=0,
                            char padChar=0) const;
 
  static unsigned checkAddition(unsigned addend1,unsigned addend2);
  static unsigned checkMultiplication(unsigned factor1,unsigned factor2);
  static unsigned overflow();
 
  static MSStringBuffer *defaultBuffer();
#if 0
  static MSStringBuffer *fromContents(const char *pBuffer);
#endif
  
/*-------------------------------- Accessing -----------------------------------
| The following functions provide access to various attributes of a buffer:    |
|   null          - Returns the address of the null buffer.                    |
|   useCount      - Returns the number of MSStrings that refer to the buffer.  |
|   length        - Returns the length of the buffer's contents.               |
|   contents      - Returns the address of the buffer's contents.              |
|   defaultBuffer - Static member function that returns the address of the     |
|                   null buffer for the class.                                 |
|   fromContents  - Static member function that returns the address of         |
|                   MSStringBuffer, given a pointer to its contents.           |
|   charType      - Returns the type of a character at a given index.          |
|   next          - Returns a pointer to the next character, not the next      |
|                   byte, in the buffer.                                       |
------------------------------------------------------------------------------*/
  MSStringBuffer *null() const;
  
  unsigned useCount() const;
  unsigned length() const;
  
  const char *contents() const;
  char *contents();
  
  virtual MSStringEnum::CharType charType(unsigned index) const;
  virtual char *next(const char *prev);
  virtual const char *next(const char *prev) const;
 
/*---------------------------- Reference Counting ------------------------------
| The following functions manage the buffer reference count:                   |
|   addRef    - Increments the usage count.                                    |
|   removeRef - Decrements the usage count and deletes the buffer when the     |
|               usage count goes to 0.                                         |
------------------------------------------------------------------------------*/
  MSStringBuffer *addRef();
  void removeRef();
  void unSafeRemoveRef(void) { if (--refs==0) delete this; }

/*-------------------------------- Comparing -----------------------------------
| The following functions permit the MSStringBuffer's contents to be compared  |
| to some other character array:                                               |
|   Comparison - Enumeration of the possible return codes from                 |
|                MSStringBuffer::compare.                                      |
|   compare    - Compares the buffer to an argument character array.           |
------------------------------------------------------------------------------*/
  enum Comparison {equal,greaterThan,lessThan};
  virtual Comparison compare(const void *p,unsigned  len) const;
 
/*-------------------------------- Character Subsetting ------------------------
| The following function is used when a subset of characters is required:      |
|   subString - Returns a new MSStringBuffer, of the same type as the previous |
|               one, that contains the specified subset of characters.         |
------------------------------------------------------------------------------*/
  virtual MSStringBuffer *subString(unsigned startPos,unsigned len,char padCharacter) const;
 
/*-------------------------------- Diagnostics ---------------------------------
| The following function provides diagnostic information about the buffer:     |
|   asDebugInfo - Returns information about the buffer's internal              |
|                 representation that can be used for debugging.               |
------------------------------------------------------------------------------*/
  MSString asDebugInfo() const;
 
/*--------------------------------- Testing ------------------------------------
| The following functions are called by the corresponding MSString functions   |
| to test the buffer's contents:                                               |
|   isAlphanumeric- See MSString::isAlphanumeric.                              |
|   isAlphabetic  - See MSString::isAlphabetic.                                |
|   isASCII       - See MSString::isASCII.                                     |
|   isControl     - See MSString::isControl.                                   |
|   isDigits      - See MSString::isDigits.                                    |
|   isGraphics    - See MSString::isGraphics.                                  |
|   isHexDigits   - See MSString::isHexDigits.                                 |
|   isLowerCase   - See MSString::isLowerCase.                                 |
|   isPrintable   - See MSString::isPrintable.                                 |
|   isPunctuation - See MSString::isPunctuation.                               |
|   isUpperCase   - See MSString::isUpperCase.                                 |
|   isWhiteSpace  - See MSString::isWhiteSpace.                                |
|   isMBCS        - See MSString::isMBCS.                                      |
|   isSBCS        - See MSString::isSBCS.                                      |
|   isValidMBCS   - See MSString::isValidMBCS.                                 |
|   includesMBCS  - See MSString::includesMBCS.                                |
|   includesSBCS  - See MSString::includesSBCS.                                |
------------------------------------------------------------------------------*/
  MSBoolean isAlphanumeric() const;
  MSBoolean isAlphabetic() const;
  MSBoolean isASCII () const;
  MSBoolean isControl() const;
  MSBoolean isDigits() const;
  MSBoolean isGraphics() const;
  MSBoolean isHexDigits() const;
  MSBoolean isLowerCase() const;
  MSBoolean isPrintable() const;
  MSBoolean isPunctuation() const;
  MSBoolean isUpperCase() const;
  MSBoolean isWhiteSpace() const;

  // these need to be virtual so that the MSMBStringBuffer can deal with
  // them correctly.
  virtual MSBoolean isMBCS() const;  
  virtual MSBoolean isSBCS() const;
  virtual MSBoolean isValidMBCS() const;  
  virtual MSBoolean includesMBCS() const;  
  virtual MSBoolean includesSBCS() const;

  // these methods are here for backward compatibility - use MBCS methods
  MSBoolean isDBCS() const;
  MSBoolean isValidDBCS() const;
  MSBoolean includesDBCS() const;
  
/*-------------------------------- Searching -----------------------------------
| The following functions are called by the corresponding MSString             |
| functions to search the buffer's contents:                                   |
|   indexOf           - See MSString::indexOf.                                 |
|   indexOfAnyBut     - See MSString::indexOfAnyBut.                           |
|   indexOfAnyOf      - See MSString::indexOfAnyOf.                            |
|   lastIndexOf       - See MSString::lastIndexOf.                             |
|   lastIndexOfAnyBut - See MSString::lastIndexOfAnyBut.                       |
|   lastIndexOfAnyOf  - See MSString::lastIndexOfAnyOf.                        |
------------------------------------------------------------------------------*/
  virtual unsigned indexOf(const char *pString,unsigned len,unsigned startPos=0) const;
  virtual unsigned indexOf(const MSStringTest &aTest,unsigned startPos=0) const;
  virtual unsigned indexOfAnyBut(const char *pString,unsigned len,unsigned startPos=0) const;
  virtual unsigned indexOfAnyBut(const MSStringTest &aTest,unsigned startPos=0) const;
  virtual unsigned indexOfAnyOf (const char *pString,unsigned len,unsigned startPos=0) const;
  virtual unsigned indexOfAnyOf (const MSStringTest &aTest,unsigned startPos=0) const;

  virtual unsigned lastIndexOf(const char *pString,unsigned len,unsigned startPos=UINT_MAX-1) const;
  virtual unsigned lastIndexOf(const MSStringTest &aTest,unsigned startPos=UINT_MAX-1) const;
  virtual unsigned lastIndexOfAnyBut(const char *pString,unsigned len,unsigned startPos=UINT_MAX-1) const;
  virtual unsigned lastIndexOfAnyBut(const MSStringTest &aTest,unsigned startPos=UINT_MAX-1) const;
  virtual unsigned lastIndexOfAnyOf(const char *pString,unsigned len,unsigned startPos=UINT_MAX-1) const;
  virtual unsigned lastIndexOfAnyOf(const MSStringTest &aTest,unsigned startPos=UINT_MAX-1) const;
 
/*--------------------------------- Editing ------------------------------------
  The following functions are called by the corresponding MSString functions
  to edit the buffer's contents:
    center       - See MSString::center.
    change       - See MSString::change.
    copy         - See MSString::copy.
    drop         - See MSString::drop.
    insert       - See MSString::insert.
    leftJustify  - See MSString::leftJustify.
    lowerCase    - See MSString::lowerCase.
    overlayWith  - See MSString::overlayWith.
    remove       - See MSString::remove.
    reverse      - See MSString::reverse.
    rotate       - See MSString::rotate.
    rightJustify - See MSString::rightJustify.
    strip        - See MSString::strip.
    take         - See MSString::take.
    translate    - See MSString::translate.
    upperCase    - See MSString::upperCase.
------------------------------------------------------------------------------*/
  virtual MSStringBuffer *center(unsigned newLen,char  padCharacter);
  virtual MSStringBuffer *change(const char *pSource,unsigned sourceLen,
                                 const char *pTarget,unsigned targetLen,
                                 unsigned startPos,unsigned numChanges);
  virtual MSStringBuffer *copy(unsigned numCopies);
  virtual MSStringBuffer *drop(int count);
  virtual MSStringBuffer *insert(const char *pInsert,unsigned insertLen,
                                 unsigned pos,char padCharacter);
  virtual MSStringBuffer *leftJustify(unsigned newLen,char  padCharacter);
  virtual MSStringBuffer *lowerCase();
  virtual MSStringBuffer *overlayWith(const char *overlay,unsigned len,
                                      unsigned pos,char padCharacter);
  virtual MSStringBuffer *remove(unsigned startPos,unsigned numChars);
  virtual MSStringBuffer *reverse();
  virtual MSStringBuffer *rotate(int count);
  virtual MSStringBuffer *rightJustify(unsigned newLen,char  padCharacter);
  virtual MSStringBuffer *strip(const char *pChars,unsigned len,MSStringEnum::StripMode mode);
  virtual MSStringBuffer *strip(const MSStringTest &aTest,MSStringEnum::StripMode mode);
  virtual MSStringBuffer *take(int count);
  virtual MSStringBuffer *translate(const char *pInputChars,unsigned inputLen,
                                    const char *pOutputChars,unsigned outputLen,char padCharacter);
  virtual MSStringBuffer *upperCase();

protected:
/*------------------------------ Implementing ----------------------------------
  The following functions provide utilities used to implement this class:
    startSearch          - Initializes an indexOf search.
    startBackwardsSearch - Initializes a lastIndexOf search.
    allocate             - Returns a new buffer of the required length.
    operator new         - Allocates space for a buffer of the specified
                           length.
    operator delete      - Deallocates a buffer.
    initialize           - Static member function used to initialize (set up
                           null buffer, DBCS table, and so forth).
    className            - Returns the name of the class (MSStringBuffer).
------------------------------------------------------------------------------*/
  virtual unsigned startSearch(unsigned startPos,unsigned searchLen) const;
  virtual unsigned startBackwardsSearch(unsigned startPos,unsigned searchLen) const;
 
  virtual MSStringBuffer *allocate(unsigned bufLength) const;
 
  void *operator new(size_t t,unsigned bufLen);
  void operator delete(void *p);
  
  static MSStringBuffer *initialize();
  
  virtual const char *className() const;
 
/*-------------------------- Constructor/Destructor ----------------------------
The constructor for this class requires the length of the buffer(the
value to be stored in the length data member).
------------------------------------------------------------------------------*/
  MSStringBuffer(unsigned newLen);
  ~MSStringBuffer();
 
private:
/*--------------------------------- Private ----------------------------------*/
#ifdef MS_MULTI_THREAD    
  MSMutex _mutex;      // buffer lock, protects reference counting
#endif

  unsigned refs;
  unsigned len;
  char     data[1];



};

/*---------------------------- Inline Methods --------------------------------*/

#ifndef MS_NO_INLINES
#ifndef MSStringBufferINLINES
#include <MSTypes/MSStringBufferInlines.C>
#endif
#endif

#endif
