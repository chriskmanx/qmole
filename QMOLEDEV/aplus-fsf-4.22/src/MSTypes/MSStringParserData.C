///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

/*******************************************************************************
* FILE NAME: MSStringParserData.C                                              *
*                                                                              *
* DESCRIPTION:                                                                 *
*   This file contains the implementation of classes/functions declared        *
*   in MSStringParserData.H                                                    *
*******************************************************************************/
extern "C"
{
#include <limits.h>
#include <string.h>
}

#include <MSTypes/MSStringParserData.H>

/*------------------------------------------------------------------------------
| Define whiteSpace characters                                                 |
------------------------------------------------------------------------------*/
static const char whiteSpace[]="\t\n\v\f\r ";
static const MSString emptyString;

/*------------------------------------------------------------------------------
| MSStringParserData::MSStringParserData                                       |
------------------------------------------------------------------------------*/
MSStringParserData::MSStringParserData(const MSString &aString_) :
_parseText(aString_),
_currentPosition(0),
_patternPosition(0),
_patternLength(0),
_tokenArraySize(0),
_tokenCount(0),
_lastToken(0),
_lastSkip(MSFalse),
_usedTokens(0),
_refs(1)
{
}

/*------------------------------------------------------------------------------
| MSStringParserData::~MSStringParserData                                      |
------------------------------------------------------------------------------*/
MSStringParserData::~MSStringParserData(void)
{
  if (_usedTokens) delete _usedTokens;
}

/*------------------------------------------------------------------------------
| MSStringParserData::processToken                                             |
|                                                                              |
| Parse the remaining string data into the token.  If this is a token reparse  |
| then do not save the token(since it will not be parsed again).               |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::processToken(MSString &aToken_,MSBoolean reparse_)
{
  reparseLastToken();
  if (_currentPosition>=_parseText.length()) aToken_=emptyString;
  else aToken_=_parseText.subString(_currentPosition);
  
  _lastToken=&aToken_;
  _lastSkip=MSFalse;
  if (reparse_==MSFalse) saveToken(_lastToken);
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParserData::processPattern(const MSString&)                          |
|                                                                              |
| Searches for the specified pattern(from the end of the last pattern) and     |
| re-parses the string that preceded the match.  If the pattern was not found  |
| position at end of string.                                                   |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::processPattern(const MSString& aPattern_)
{
  unsigned startPos=_patternPosition+_patternLength;
  _patternPosition=_parseText.indexOf(aPattern_,startPos);

  if (_patternPosition<_parseText.length())
   {
     _patternLength=aPattern_.length();
     if (_tokenCount>0) reparseTokens(startPos,_patternPosition);
   }
  else
   {
     _patternPosition=_parseText.length();
     _patternLength=0;
   }

  _currentPosition=_patternPosition+_patternLength;
  clearSavedTokens();

  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParserData::processPattern(const char *)                             |
|                                                                              |
| Searches for the specified pattern(from the end of the last pattern) and     |
| re-parses the aString_ that preceded the match.  If the pattern was not      |
| found position at end of string.                                             |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::processPattern(const char *pPattern_)
{
  unsigned startPos=_patternPosition+_patternLength;
  _patternPosition=_parseText.indexOf(pPattern_,startPos);

  if (_patternPosition<_parseText.length())
   {
     _patternLength=strlen(pPattern_);
     if (_tokenCount>0) reparseTokens(startPos,_patternPosition);
   }
  else
   {
     _patternPosition=_parseText.length();
     _patternLength=0;
   }

  _currentPosition=_patternPosition+_patternLength;
  clearSavedTokens();

  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParserData::processPattern(char)                                     |
|                                                                              |
| Searches for the specified pattern(from the end of the last pattern) and     |
| re-parses the aString_ that preceded the match.  If the pattern was not      |
| found position at end of string aString_.                                    |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::processPattern(char aPatternChar_)
{
  unsigned startPos=_patternPosition+_patternLength;
  _patternPosition=_parseText.indexOf(aPatternChar_,startPos);

  if (_patternPosition<_parseText.length())
   {
     _patternLength=1;
     if (_tokenCount>0) reparseTokens(startPos,_patternPosition);
   }
  else
   {
     _patternPosition=_parseText.length();
     _patternLength=0;
   }

  _currentPosition=_patternPosition+_patternLength;
  clearSavedTokens();

  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParserData::processSkip                                              |
|                                                                              |
| Parse the remaining string data as if a token was specified,except in this   |
| case the token does not exist.  If this is a reparse_ of a skip,do not save  |
| the skip marker in the token list.                                           |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::processSkip(MSBoolean reparse_)
{
  reparseLastToken();
  _lastToken=0;
  _lastSkip=MSTrue;
  if (!reparse_) saveToken(0);

  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParserData::processTest                                              |
|                                                                              |
| Use the MSStringTest object to set the parse position                        |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::processTest(const MSStringTest &aStringTest_)
{
  unsigned position=_parseText.indexOf(aStringTest_,_patternPosition+_patternLength);
  if (position==_parseText.length()) position=_parseText.length();
  return setPosition(position);
}

/*------------------------------------------------------------------------------
| MSStringParserData::setPosition                                              |
|                                                                              |
| Sets the current location for parsing to the position specified in the       |
| original parse aString_ string.  Since positioning acts like a pattern match,|
| the pattern position will be updated to reflect the positioning.             |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::setPosition(unsigned position_)
{
  if (position_>=_parseText.length())
   {
     _patternPosition=_parseText.length();
   }
  else
   {
     unsigned startPos=_patternPosition+_patternLength;
     if ((position_>startPos)&&(_tokenCount>0)) reparseTokens(startPos,position_);
     _patternPosition=position_;
   }

  _patternLength=0;
  _currentPosition=_patternPosition;
  clearSavedTokens();

  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParserData::changePosition(using signed value)                       |
|                                                                              |
| Sets the current location for parsing to the relative position specified.    |
| For negative values,do the processing here with underflow checking.  For     |
| positive values,use the unsigned delta_ method.                              |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::changePosition(int delta_)
{
  if (delta_<0)
   {
     unsigned newPosition;
     if ((_patternPosition<(-delta_))||(delta_==INT_MIN)) newPosition=0;
     else newPosition=_patternPosition+delta_;

     if ((_patternLength>0)&&(_tokenCount>0)) reparseTokens(_patternPosition,_parseText.length());

     _patternLength=0;
     setPosition(newPosition);
   }
  else changePosition((unsigned)delta_);
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParserData::changePosition(using unsigned value)                     |
|                                                                              |
| Sets the current location for parsing to the relative position specified     |
| with overflow checking.                                                      |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::changePosition(unsigned delta_)
{
  unsigned newPosition=_patternPosition+delta_;
  if (_patternPosition>(UINT_MAX-delta_)) newPosition=_parseText.length();

  if ((_patternLength>0)&&(_tokenCount>0)&&(newPosition>=_parseText.length()))
   {
     reparseTokens(_patternPosition,_parseText.length());
   }
  _patternLength=0;
  return setPosition(newPosition);
}

/*------------------------------------------------------------------------------
| MSStringParserData::reparseLastToken                                         |
|                                                                              |
| This method is used to reparse the last parsed token so that it only         |
| contains the next word and then positions around the parsed word. If no      |
| word is found,set the token to an empty string and position at end of        |
| aString_.                                                                    |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::reparseLastToken(void)
{
  if (_lastToken)
   {
     unsigned startPos=_parseText.indexOfAnyBut(whiteSpace,_currentPosition);
     if (startPos<_parseText.length())
      {
	unsigned stopPos=_parseText.indexOfAnyOf(whiteSpace,startPos);
	if (stopPos<_parseText.length())
	 {
	   if (startPos==_currentPosition) _lastToken->remove(stopPos-startPos+1);
	   else *_lastToken=_parseText.subString(startPos,stopPos-startPos);
	   _currentPosition=stopPos+1;
	 }
	else
	 {
	   if (startPos>_currentPosition) *_lastToken=_parseText.subString(startPos);
	   _currentPosition=_parseText.length();
	 }
      }
     else
      {
	*_lastToken=emptyString;
	_currentPosition=_parseText.length();
      }
   }

  if (_lastSkip)
   {
     unsigned startPos=_parseText.indexOfAnyBut(whiteSpace,_currentPosition);
     if (startPos<_parseText.length())
      {
	unsigned stopPos=_parseText.indexOfAnyOf(whiteSpace,startPos);
	if (stopPos<_parseText.length()) _currentPosition=stopPos+1;
	else _currentPosition=_parseText.length();
      }
     else _currentPosition=_parseText.length();
   }

  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParserData::reparseTokens                                            |
|                                                                              |
| This method is used to reparse all saved tokens. A reparse value of MSTrue is|
| used when calling processToken and processSkip so that the token pointer or  |
| skip marker is not saved for later parsing.                                  |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::reparseTokens(unsigned startPos_,unsigned stopPos_)
{
  MSStringParserData preText(_parseText.subString(startPos_,stopPos_-startPos_));
  for(int cursor=0;cursor<_tokenCount;cursor++)
   {
     MSString *token=_usedTokens[cursor];
     if (token!=0) preText.processToken(*token,MSTrue);
     else preText.processSkip(MSTrue);
   }
  clearSavedTokens();
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParserData::saveToken                                                |
|                                                                              |
| This method saves a token pointer or skip marker so that it can be reparsed  |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::saveToken(MSString *pToken_)
{
  if (!_usedTokens)
   {
     _usedTokens=new MSTokenPointer[10];
     _tokenArraySize=10;
   }

  if (_tokenArraySize<=_tokenCount+1)
   {
     MSTokenPointer *oldList=_usedTokens;
     _usedTokens=new MSTokenPointer[_tokenArraySize+10];
     for(int tmp=0;tmp<_tokenCount;tmp++) _usedTokens[tmp]=oldList[tmp];
     delete oldList;
     _tokenArraySize+=10;
   }

  _usedTokens[_tokenCount]=pToken_;
  _tokenCount++;

  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParserData::clearSavedTokens                                         |
|                                                                              |
| This method clears all references to saved tokens                            |
------------------------------------------------------------------------------*/
MSStringParserData& MSStringParserData::clearSavedTokens(void)
{
  _tokenCount=0;
  _lastToken=0;
  _lastSkip=MSFalse;
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParserData::addRef                                                   |
|                                                                              |
| Add a reference by incrementing the reference count                          |
------------------------------------------------------------------------------*/
void MSStringParserData::addRef(void)
{
  _refs++;
}

/*------------------------------------------------------------------------------
| MSStringParserData::removeRef                                                |
|                                                                              |
| Remove a reference by decrementing the reference count. If the refernce      |
| count goes to 0 then delete this.                                            |
------------------------------------------------------------------------------*/
void MSStringParserData::removeRef(void)
{
  if (--_refs==0) delete this;
}


