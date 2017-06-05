#ifndef MSStringParserDataHEADER
#define MSStringParserDataHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSStringHEADER
#include <MSTypes/MSString.H>
#endif

/*----------------------------------------------------------------------------*/
/* Define token pointer type                                                  */
/*----------------------------------------------------------------------------*/
typedef MSString* MSTokenPointer;

class MSTypesExport MSStringParserData
{
/*******************************************************************************
* Objects of this class are used to store and manage the data for parsing      *
* strings.                                                                     *
*******************************************************************************/
public:
/*------------------------- Constructor and Destructor -------------------------
| The constructor accepts the text string to be parsed and initialized the     |
| parse history information.  The parse history information is used to track   |
| what parsing has been performed(but may not be the final results).           |
|                                                                              |
| The destructor is used to delete possible parse history information.         |
------------------------------------------------------------------------------*/
  MSStringParserData(const MSString&);
 ~MSStringParserData(void);

/*--------------------------------- Parsing ------------------------------------
| These methods are used to perform the actual string parsing and to store a   |
| record of the actions performed.  These methods are intended to be called    |
| only by MSStringParse objects.                                               |
------------------------------------------------------------------------------*/
  MSStringParserData& processToken(MSString& aToken_,MSBoolean reparse_=MSFalse);
  MSStringParserData& processPattern(const MSString& aPattern_);
  MSStringParserData& processPattern(const char *pPattern_);
  MSStringParserData& processPattern(char aPatternChar_);
  MSStringParserData& processSkip(MSBoolean reparse_=MSFalse);
  MSStringParserData& processTest(const MSStringTest&);
  MSStringParserData& setPosition(unsigned position_);
  MSStringParserData& changePosition(int delta_);
  MSStringParserData& changePosition(unsigned delta_);

/*------------------------------ Implementation --------------------------------
| The following methods are used to implement common actions performed during  |
| string parsing.                                                              |
------------------------------------------------------------------------------*/
  MSStringParserData& reparseLastToken(void);
  MSStringParserData& reparseTokens(unsigned startPos,unsigned stopPos);
  MSStringParserData& saveToken(MSString *token);
  MSStringParserData& clearSavedTokens(void);

/*---------------------------- Reference Counting ------------------------------
| The following functions manage the buffer reference count:                   |
|   addRef   -Increments the usage count.                                      |
|   removeRef-Decrements the usage count and deletes the buffer when the       |
|               usage count goes to 0.                                         |
------------------------------------------------------------------------------*/
  void addRef(void);
  void removeRef(void);

/*--------------------------------- PRIVATE ----------------------------------*/
private:
  MSString        _parseText;
  unsigned        _currentPosition;
  unsigned        _patternPosition;
  unsigned        _patternLength;
  unsigned        _tokenCount;
  unsigned        _tokenArraySize;
  MSString       *_lastToken;
  MSBoolean       _lastSkip;
  MSTokenPointer *_usedTokens;
  unsigned        _refs;
};

#endif 
