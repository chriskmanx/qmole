#ifndef MSStringParserHEADER
#define MSStringParserHEADER

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

class MSStringParserData;

class MSTypesExport MSStringParser
{
/*******************************************************************************
* Objects of this class enable you to "parse" the contents of MSStrings and    *
* place portions of the string into other strings.  You can control the        *
* parsing of the string by specifying patterns that must be matched and        *
* by specifying relative and absolute column numbers.  
*                                                                              *
* You usually will not create MSStringParser objects explicitly.  Instead,you  *
* will create them implicitly by applying the right-shift operator to an       *
* MSString.  MSStringParser also provides the right-shift operator as a member *
* function so that you can chain together invocations of the operator.  For    *
* example,a typical expression that uses MSStringParser objects might look     *
* like:                                                                        *
*    aFileName>>drive>>':'>>path;                                              *
*                                                                              *
* The right shift operator does one of four things,depending on the            *
* type of the right-hand operand:                                              *
*                                                                              *
*   MSString- If the argument is an MSString,then the string parser object     *
*             will set this string to the next token from the text being       *
*             parsed.                                                          *
*                                                                              *
*   pattern-  If the argument is a "pattern" then the parser advances to the   * 
*             next character beyond the occurrence of that pattern in its      *
*             text.  The pattern can be any of:                                *
*               const char*   - Searches for the sequence of characters        *
*                                described by the character array.             *
*               const MSString - Searches for the sequence of characters       *
*                                described by the string.  Note that the       *
*                                treatment of a const MSString is              *
*                                fundamentally different than the treatment    *
*                                of a non-const one!                           *
*               char          - Searches for the next occurrence of the        *
*                                argument character.                           *
*               MSStringTest   - Searches for the next character in the        *
*                                text for which the string test object         *
*                                returns MSTrue.                               *
*                                                                              *
*   number -  If the argument is a number then the current parser text         *
*             position is adjusted by the argument amount.  The value can      *
*             be positive or negative.                                         *
*                                                                              *
*   special-  MSStringParser defines some special right-shift operands that    *
*             perform special purpose parser operations:                       *
*               MSStringParser::reset- This enumeration value resets the       *
*                                      parser text position to 0.              *
*               MSStringParser::skip - This enumeration value skips one        *
*                                      token in the text.  It is equivalent    *
*                                      to ">> temp" where temp is a temporary  *
*                                      MSString that is discarded.             *
*               MSStringParser::Skip - An object of this class skips a given   *
*                                      number of tokens.                       *
*                                                                              *
* You can also use the left-shift operator with an unsigned numeric argument.  *
* This will cause the parser object to reposition to the column specified      *
* by the argument.  Note that the argument is not relative as it is in the     *
* case of the right-shift operator.  Instead,it is an abosulte column          *
* position.                                                                    *
*******************************************************************************/
public:
  ~MSStringParser(void);

/*------------------------- Special Parsing Operands ---------------------------
| These types permit special-purpose parsing techniques:                       |
|   Command-  Enumeration defining special-purpose parsing tokens.             |
|               reset- Resets the parser position to 0.                        |
|               skip - Causes the parser to skip one token(word) in the        |
|                       input text.                                            |
|   Skip   -  Object used to skip a given number of words(tokens) in the       |
|             input text without assigning those words to output strings.      |
------------------------------------------------------------------------------*/
enum Command {reset,skip};

class MSTypesExport Skip
{
public:
  Skip(unsigned numWords=1);
  unsigned numWords(void) const;
private:
  unsigned _numSkip;
};

/*-------------------------- Parsing Initialization ----------------------------
| These operators let you construct parser objects from strings.  In effect,   |
| they implicitly construct an MSStringParser object for you when you apply    |
| the ">>" or "<<" operators to a string.  An expression of the form:          |
|   aString>>x                                                                 |
|      -or-                                                                    |
|   aString<<n                                                                 |
| is equivalent to                                                             |
|   MSStringParser(aString)>>x                                                 |
|      -or-                                                                    |
|   MSStringParser(aString)<<n                                                 | 
|                                                                              |
| These functions are declared as friends simply so that their declaration     |
| appears within this class declaration.                                       |
------------------------------------------------------------------------------*/
  friend MSTypesExport MSStringParser operator>>(const MSString&,MSString& aToken_);
  friend MSTypesExport MSStringParser operator>>(const MSString&,const MSString& aPattern_);
  friend MSTypesExport MSStringParser operator>>(const MSString&,const char *pPattern_);
  friend MSTypesExport MSStringParser operator>>(const MSString&,char aCharPattern_);
  friend MSTypesExport MSStringParser operator>>(const MSString&,unsigned position_);
  friend MSTypesExport MSStringParser operator>>(const MSString&,int position_);
  friend MSTypesExport MSStringParser operator<<(const MSString&,unsigned position_);
						  
  friend MSTypesExport MSStringParser operator>>(const MSString&,const MSStringTest&);
  friend MSTypesExport MSStringParser operator>>(const MSString&,MSStringParser::Command);
  friend MSTypesExport MSStringParser operator>>(const MSString&,const MSStringParser::Skip&);

/*-------------------------------- Tokenizing ----------------------------------
| This function parses the next "token" from the parser object and places      |
| it into the MSString operand.  By necessity,this operatorplaces the rest     |
| of the parser text into the string.  When the parser encounters a subsequent |
| parsing instructions it will go back and adjust the token placed into the    |
| string.                                                                      |
|                                                                              |
| For example:                                                                 |
|   "token1 token2">>token1   // token1=="token1 token2" at this point         |
|                  >>token2;  // token2=="token2" and                          |
|                             // token1=="token1".                             |
------------------------------------------------------------------------------*/
  MSStringParser& operator>>(MSString& aToken_);

/*----------------------------- Pattern Matching -------------------------------
| These operators advance to the next occurrence of the argument pattern       |
| in the parser text.  Upon return,the parser will be positioned at the        |
| next character beyond the text that matched the pattern.  If the pattern     |
| is not found,the parser will be positioned off the end of the text.          |
|                                                                              |
| Note that when using and MSString as a pattern you should cast it to a const |
| MSString reference.                                                          |
------------------------------------------------------------------------------*/
  MSStringParser& operator>>(const MSString& aPattern_);
  MSStringParser& operator>>(const char *pPattern_);
  MSStringParser& operator>>(char aCharPattern_);
  MSStringParser& operator>>(const MSStringTest&);

/*----------------------- Relative Column Positioning --------------------------
| These operators move the parser text position relative to its current        |
| position.  A negative argument moves backward,a positive argument forward.   |
| The adjustment is made starting at the point at which the prior parsing      |
| instruction *started*.  For example:                                         |
|    "1234">>token1>>1>>token2>>2>>token3;                                     |
| will result in:                                                              |
|    token1=="1"                                                               |
|    token2=="23"                                                              |
|    token3=="4".                                                              |
------------------------------------------------------------------------------*/
  MSStringParser& operator>>(int delta_);
  MSStringParser& operator>>(unsigned delta_);

/*----------------------- Absolute Column Positioning --------------------------
| These operators reset the parser text position to an absolute column         |
| number.                                                                      |
------------------------------------------------------------------------------*/
  MSStringParser& operator<<(unsigned position_);

/*----------------------------- Special Parsing --------------------------------
| These operators handle the special commands and skip objects                 |
------------------------------------------------------------------------------*/
  MSStringParser& operator>>(Command);
  MSStringParser& operator>>(const Skip&);

protected:
/*------------------------- Constructors/Destructor ----------------------------
| You can construct a parser object by providing:                              |
|   o a string that defines the text to be parsed                              |
|   o from an existing parser object(copy constructor)                         |
|                                                                              |
| Note that usually you will construct parser objects by applying the          |
| right-shift operatorto a string.  The constructor is protected to prevent    |
| you from creating objects except via use of those operators.  Creation is    |
| prevented because of the nature of string parser objects.  Since they hold   |
| references to operands,it is unwise to permit the objects to persist beyond  |
| the scope of those operands.                                                 |
------------------------------------------------------------------------------*/
  MSStringParser(const MSString&);
  MSStringParser(const MSStringParser&);

/*--------------------------------- PRIVATE ----------------------------------*/
private:
  MSStringParserData *_parseData;
}; 

#endif 





