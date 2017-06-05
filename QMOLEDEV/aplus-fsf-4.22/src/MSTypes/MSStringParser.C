///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

/*******************************************************************************
* FILE NAME: MSStringParser.C                                                  *
*                                                                              *
* DESCRIPTION:                                                                 *
*   This file contains the implementation of classes/functions declared        *
*   in MSStringParser.H                                                        *
*******************************************************************************/
#include <MSTypes/MSStringTest.H>
#include <MSTypes/MSStringParserData.H>
#include <MSTypes/MSStringParser.H>

/*------------------------------------------------------------------------------
| MSStringParser::~MSStringParser                                              |
------------------------------------------------------------------------------*/
MSStringParser::~MSStringParser(void)
{ if(_parseData) _parseData->removeRef(); }

/*------------------------------------------------------------------------------
| operator>>(const MSString& aString_,MSString aToken_)                        |
|                                                                              |
| Create an MSStringParser object from a parse string and a aToken_            |
------------------------------------------------------------------------------*/
MSStringParser operator>>(const MSString& aString_,MSString& aToken_)
{ return(MSStringParser(aString_)>>aToken_); }

/*------------------------------------------------------------------------------
| operator>>(const MSString& aString_,const MSString aPattern_)                |
|                                                                              |
| Create an MSStringParser object from a parse string and a aPattern_          |
------------------------------------------------------------------------------*/
MSStringParser operator>>(const MSString& aString_,const MSString& aPattern_)
{ return(MSStringParser(aString_)>>aPattern_); }

/*------------------------------------------------------------------------------
| operator>>(const MSString& aString_,const char *pPattern_)                   |
|                                                                              |
| Create an MSStringParser object from a parse string and a pPattern_          |
------------------------------------------------------------------------------*/
MSStringParser operator>>(const MSString& aString_,const char *pPattern_)
{ return(MSStringParser(aString_)>>pPattern_); }

/*------------------------------------------------------------------------------
| operator>>(const MSString& aString_,char patternChar_)                       |
|                                                                              |
| Create an MSStringParser object from a parse string and a patternChar_       |
------------------------------------------------------------------------------*/
MSStringParser operator>>(const MSString& aString_,char patternChar_)
{ return(MSStringParser(aString_)>>patternChar_); }

/*------------------------------------------------------------------------------
| operator>>(const MSString& aString_,unsigned position_)                      |
|                                                                              |
| Create an MSStringParser object from a parse string and a position           |
------------------------------------------------------------------------------*/
MSStringParser operator>>(const MSString& aString_,unsigned position_)
{ return(MSStringParser(aString_)>>position_); }

/*------------------------------------------------------------------------------
| operator>>(const MSString& aString_,int position_)                           |
|                                                                              |
| Create an MSStringParser object from a parse string and a position           |
------------------------------------------------------------------------------*/
MSStringParser operator>>(const MSString& aString_,int position_)
{ return(MSStringParser(aString_)>>position_); }

/*------------------------------------------------------------------------------
| operator<<(const MSString& aString_,unsigned position_)                      |
|                                                                              |
| Create an MSStringParser object from a parse string and a position           |
------------------------------------------------------------------------------*/
MSStringParser operator<<(const MSString& aString_,unsigned position_)
{ return(MSStringParser(aString_)<<position_); }

/*------------------------------------------------------------------------------
| operator<<(const MSString& aString_,const MSStringText& aStringTest_)        |
|                                                                              |
| Create an MSStringParser object from a parse string and a string test        |
------------------------------------------------------------------------------*/
MSStringParser operator>>(const MSString& aString_,const MSStringTest& aStringTest_)
{ return(MSStringParser(aString_)>>aStringTest_); }

/*------------------------------------------------------------------------------
| MSStringParser::operator>>(MSString& aToken_)                                |
------------------------------------------------------------------------------*/
MSStringParser& MSStringParser::operator>>(MSString& aToken_)
{
  _parseData->processToken(aToken_);
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParser::operator>>(const MSString& aPattern_)                        |
------------------------------------------------------------------------------*/
MSStringParser& MSStringParser::operator>>(const MSString& aPattern_)
{
  _parseData->processPattern(aPattern_);
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParser::operator>>(const char *pPattern_)                            |
------------------------------------------------------------------------------*/
MSStringParser& MSStringParser::operator>>(const char *pPattern_)
{
  _parseData->processPattern(pPattern_);
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParser::operator>>(char patternChar_)                                |
------------------------------------------------------------------------------*/
MSStringParser& MSStringParser::operator>>(char patternChar_)
{
  _parseData->processPattern(patternChar_);
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParser::operator>>(const MSStringTest& test)                         |
------------------------------------------------------------------------------*/
MSStringParser& MSStringParser::operator>>(const MSStringTest& test)
{
  _parseData->processTest(test);
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParser::operator>>(int delta_)                                       |
------------------------------------------------------------------------------*/
MSStringParser& MSStringParser::operator>>(int delta_)
{
  _parseData->changePosition(delta_);
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParser::operator>>(unsigned delta_)                                  |
------------------------------------------------------------------------------*/
MSStringParser& MSStringParser::operator>>(unsigned delta_)
{
  _parseData->changePosition(delta_);
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParser::operator<<(unsigned position_)                               |
------------------------------------------------------------------------------*/
MSStringParser& MSStringParser::operator<<(unsigned position_)
{
  _parseData->setPosition(position_);
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParser::Skip::Skip                                                   |
------------------------------------------------------------------------------*/

MSStringParser::Skip::Skip(unsigned numWords) :
_numSkip(numWords)
{
}

/*------------------------------------------------------------------------------
| MSStringParser::Skip::numWords                                               |
------------------------------------------------------------------------------*/

unsigned MSStringParser::Skip::numWords(void) const
{ return _numSkip; }

/*------------------------------------------------------------------------------
| operator<<(const MSString& aString_,MSStringParser::Command aCommand_)       |
|                                                                              |
| Create an MSStringParser object from a parse string and a command            |
------------------------------------------------------------------------------*/
MSStringParser operator>>(const MSString& aString_,MSStringParser::Command aCommand_)
{ return(MSStringParser(aString_)>>aCommand_); }

/*------------------------------------------------------------------------------
| operator<<(const MSString& aString_,const MSStringParser::Skip& aSkipObj_)   |
|                                                                              |
| Create an MSStringParser object from a parse string and a skip object        |
------------------------------------------------------------------------------*/
MSStringParser operator>>(const MSString& aString_,const MSStringParser::Skip& aSkipObj_)
{ return(MSStringParser(aString_)>>aSkipObj_); }

/*------------------------------------------------------------------------------
| MSStringParser::operator<<(Command aCommand_)                                |
------------------------------------------------------------------------------*/
MSStringParser& MSStringParser::operator>>(Command aCommand_)
{
  switch (aCommand_)
  {
  case reset: _parseData->setPosition(0); break;
  case skip:  _parseData->processSkip();  break;
  default:                                break;
  }
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParser::operator<<(const Skip&)                                      |
------------------------------------------------------------------------------*/
MSStringParser& MSStringParser::operator>>(const Skip& aSkipObj_)
{
  unsigned skipCnt=aSkipObj_.numWords();
  while(skipCnt-->0) _parseData->processSkip();
  return *this;
}

/*------------------------------------------------------------------------------
| MSStringParser::MSStringParser                                               |
------------------------------------------------------------------------------*/
MSStringParser::MSStringParser(const MSString& aString_)
{
  _parseData=new MSStringParserData(aString_);
}

/*------------------------------------------------------------------------------
| MSStringParser::MSStringParser(copy constructor)                             |
------------------------------------------------------------------------------*/
MSStringParser::MSStringParser(const MSStringParser& aParser)
{
  _parseData=aParser._parseData;
  _parseData->addRef();
}

