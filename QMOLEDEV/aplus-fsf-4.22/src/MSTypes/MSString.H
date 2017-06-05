#ifndef MSStringHEADER
#define MSStringHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef MSStringEnumHEADER
#include <MSTypes/MSStringEnum.H>
#endif

#ifndef MSScalarModelHEADER
#include <MSTypes/MSScalarModel.H>
#endif 

#include <MSTypes/MSError.H>

extern "C"
{
#include <limits.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
}

class MSStringTest;
class MSStringBuffer;
class CharPick;
#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
class istream;
#endif

class MSSymbol;
class MSFormat;

class MSTypesExport MSString : public MSScalarModel 
{
/*******************************************************************************
* Objects of the MSString class are arrays of characters.                      *
*                                                                              *
* MSStrings provide the following function beyond that available from the      *
* standard C char* arrays and the string.h library functions:                  *
*   o  No restrictions on string contents.  In other words, strings can        *
*      contain null characters.                                                *
*   o  Automatic conversion from and to numeric types                          *
*   o  Automatic deletion of string buffer when the MSString is destroyed      *
*   o  Full support for the following:                                         *
*         - All comparison operators                                           *
*         - All bitwise operators                                              *
*         - Concatenation using the more natural "+" operator.                 *
*   o  String data testing, such as for characters, digits, and hexadecimal    *
*      digits                                                                  *
*   o  A full complement of the following:                                     *
*         - String manipulation functions, such as center, left and right      *
*           justification, stripping of leading and trailing characters,       *
*           substring deletion, and insertion of strings                       *
*         - Corresponding string manipulation functions that return a new      *
*           MSString rather than modifying the receiver                        *
*         - String searching functions, such as index of string and last       *
*           index of string.                                                   *
*   o  Word manipulation, such as index of word and search for word phrase     *
*   o  Support for mixed strings that contain both single-byte character set   *
*      (SBCS) and double-byte character set (DBCS) characters.                 *
*                                                                              *
*      When a program using MSStrings is run on OS/2J, the MSString objects    *
*      support DBCS characters within the string contents.  The various        *
*      MSString searching functions will not accidentally match an SBCS        *
*      character with the second byte of a DBCS character that happens to      *
*      have the same value.  Also, MSString functions that modify MSStrings,   *
*      such as subString, remove, and translate, will never separate two       *
*      bytes of a DBCS character.  If one of the two bytes of a DBCS           *
*      character is removed, the remaining byte will be replaced with the      *
*      appropriate pad character (if the function performing the change has    *
*      one), or a blank.                                                       *
*                                                                              *
*      Care must be taken with MSStrings that contain DBCS data to ensure that *
*      the contents are not altered in such a way as to corrupt the data.      *
*      For example, the statement:                                             *
*                                                                              *
*           aString[ n ]= 'x';                                                 *
*                                                                              *
*      would be in error if the nth character of the MSString was the first    *
*      or second byte of a DBCS character.                                     *
*******************************************************************************/
public:
/*------------------------- Constructors/Destructor ----------------------------
| You can construct instances of this class in the following ways:             |
|    - Construct a null string.                                                |
|    - Construct a string with the ASCII representation of a given numeric     |
|      value, supporting all flavors of int and double.                        |
|    - Construct a string with a copy of the given character data, supporting  |
|      ASCIIZ strings, characters, and MSStrings.  The character data passed   |
|      will be converted to its ASCII representation.                          |
|    - Construct a string with contents that consist of copies of up to three  |
|      buffers of arbitrary data (void*).  Optionally, only the length need    |
|      be provided, in which case the MSString contents are initialized to a   |
|      given pad character.  The default is a blank.                           |
------------------------------------------------------------------------------*/
  class MSTypesExport CharPick
  {
  private:
  friend class MSString;

    MSString *_pString;
    unsigned  _index;

  public:
    CharPick(MSString *pString,unsigned index);
    CharPick(const CharPick& index);

    CharPick& operator=(char aChar);
    operator char() const;

    MSString *string(void) const;
    char value(void) const;
    unsigned index(void) const;
  };

  MSString();
  MSString(const MSString& aString);

  MSString(int);
  MSString(unsigned);
  MSString(long);
  MSString(unsigned long);
  MSString(short);
  MSString(unsigned short);
  MSString(double);

  MSString(char);
  MSString(unsigned char);
  MSString(signed char);

  MSString(const char *);
  MSString(const unsigned char *);
  MSString(const signed char *);

  MSString(const void *pBuffer1,unsigned lenBuffer1,char padCharacter=' ');
  MSString(const void *pBuffer1,unsigned lenBuffer1,
          const void *pBuffer2,unsigned lenBuffer2,char padCharacter=' ');
  MSString(const void *pBuffer1,unsigned lenBuffer1,
          const void *pBuffer2,unsigned lenBuffer2,
          const void *pBuffer3,unsigned lenBuffer3,char padCharacter=' ');

  ~MSString();
/*-------------------------------- Stream I/O ----------------------------------
  The following functions provide for reading and writing MSStrings from and
  to standard C++ streams:
    operator << - Puts an MSString's contents to an output stream.
    operator >> - Puts the next whitespace-delimited word from an input
                  stream into an MSString.
    lineFrom    - Static function that returns the next line from the
                  argument input stream.  This function accepts an optional
                  line delimiter, which defaults to \n; the resulting MSString
                  contains the characters up to the next occurrence of the
                  delimiter.  The delimiter character is skipped.
------------------------------------------------------------------------------*/
  friend MSTypesExport ostream &operator<<(ostream &aStream,const MSString &aString);
  friend MSTypesExport istream &operator>>(istream &aStream,MSString &aString);
  static MSString lineFrom(istream &aStream,char delim='\n');

/*--------------------------------- Testing ------------------------------------
| The following functions test the characters that comprise the string:         |
|   isAlphanumeric    - MSTrue if and only if all characters are in             |
|                       {'A'-'Z','a'-'z','0'-'9'}.                              |
|   isAlphabetic      - MSTrue if and only if all characters are in             |
|                       {'A'-'Z','a'-'z'}.                                      |
|   isASCII           - MSTrue if and only if all characters are in {0x00-0x7F}.|
|   isControl         - MSTrue if and only if all characters are in             |
|                       {0x00-0x1F,0x7F}.                                       |
|   isDigits          - MSTrue if and only if all characters are in {'0'-'9'}.  |
|   isGraphics        - MSTrue if and only if all characters are in {0x21-0x7E}.|
|   isHexDigits       - MSTrue if and only if all characters are in             |
|                       {'0'-'9','A'-'F','a'-'f'}.                              |
|   isBinaryDigits    - MSTrue if and only if all characters are either '0' or  |
|                       '1'.                                                    |
|   isLowerCase       - MSTrue if and only if all characters are in {'a'-'z'}.  |
|   isPrintable       - MSTrue if and only if all characters are in {0x20-0x7E}.|
|   isPunctuation     - MSTrue if and only if none of the characters are        |
|                       whitespace, control characters, or alphanumeric.        |
|   isWhiteSpace      - MSTrue if and only if all characters are in             |
|                       {0x09-0x0D,0x20}.                                       |
|   isUpperCase       - MSTrue if and only if all characters are in {'A'-'Z'}.  |
|   isDBCS            - MSTrue if and only if all characters are DBCS           |
|                       (double-byte).                                          |
|   isSBCS            - MSTrue if and only if all characters are SBCS           |
|                       (single-byte).                                          |
|   isValidDBCS       - MSTrue if and only if no DBCS characters have a 0       |
|                       second byte.                                            |
|   includesDBCS      - MSTrue if and only if any characters are DBCS           |
|                       (double-byte).                                          |
|   includesSBCS      - MSTrue if and only if any characters are SBCS           |
|                       (single-byte).                                          |
|   isLike            - MSTrue if and only if the receiver matches the argument |
|                       pattern, which can contain wildcard characters.         |
|   isAbbreviationFor - MSTrue if the receiver is a valid abbreviation of the   |
|                       argument string.                                        |
|   includes          - MSTrue if the receiver contains the argument search     |
|                       string.                                                 |
------------------------------------------------------------------------------*/
  MSBoolean isAlphanumeric() const;
  MSBoolean isAlphabetic() const;
  MSBoolean isASCII() const;
  MSBoolean isControl() const;
  MSBoolean isDigits() const;
  MSBoolean isGraphics() const;
  MSBoolean isHexDigits() const;
  MSBoolean isBinaryDigits() const;
  MSBoolean isLowerCase() const;
  MSBoolean isPrintable() const;
  MSBoolean isPunctuation() const;
  MSBoolean isUpperCase() const;
  MSBoolean isWhiteSpace() const;

  MSBoolean isMBCS() const;  
  MSBoolean isDBCS() const;
  MSBoolean isSBCS() const;
  MSBoolean isValidMBCS() const;  
  MSBoolean isValidDBCS() const;
  MSBoolean includesMBCS() const;  
  MSBoolean includesDBCS() const;
  MSBoolean includesSBCS() const;
  
  MSBoolean isLike(const MSString &aPattern,char zeroOrMore='*',char anyChar='?') const;
  MSBoolean isLike(const char *pPattern,char zeroOrMore='*',char anyChar='?') const;
  
  MSBoolean isAbbreviationFor(const MSString &fullString,unsigned minAbbrevLength=0) const;
  MSBoolean isAbbreviationFor(const char *pFullString,unsigned minAbbrevLength=0) const;
  
  MSBoolean includes(const MSString &aString) const;
  MSBoolean includes(const char *pString) const;
  MSBoolean includes(char aChar) const;
  MSBoolean includes(const MSStringTest &aTest) const;

/*------------------------------- Comparison -----------------------------------
| All MSBoolean operators are defined for strings.  The following functions    |
| are overloaded so that MSStrings can be compared to objects of type char*,   |
| as well as to other MSStrings.                                               |  
|   operator!   - is this a null string                                        |
|   operator == - MSTrue if and only if the strings are identical.             |
|   operator != - MSTrue if and only if the strings are not identical.         |
|   operator <  - MSTrue if and only if the first string is less than the      |
|                 second, applying the standard collating scheme (memcmp).     |
|   operator >  - Equivalent to '!(string1 <= string2)'.                       |
|   operator <= - Equivalent to '(string1 < string2) || (string1== string2)'.  |
|   operator >= - Equivalent to '!(string1 < string2)'.                        |
------------------------------------------------------------------------------*/
  MSBoolean operator!() const;

  long compare(const MSString&) const;
  
  inline friend MSTypesExport unsigned long hash(const MSString&,unsigned long size_);
  
  friend MSTypesExport MSBoolean operator==(const MSString &string1,const MSString &string2);
  friend MSTypesExport MSBoolean operator==(const MSString &string1,const char *pString2);
  friend MSTypesExport MSBoolean operator==(const char *pString1,const MSString &string2);
  
  friend MSTypesExport MSBoolean operator!=(const MSString &string1,const MSString &string2);
  friend MSTypesExport MSBoolean operator!=(const MSString &string1,const char *pString2);
  friend MSTypesExport MSBoolean operator!=(const char *pString1,const MSString &string2);
  
  friend MSTypesExport MSBoolean operator<(const MSString &string1,const MSString &string2);
  friend MSTypesExport MSBoolean operator<(const MSString &string1,const char *pString2);
  friend MSTypesExport MSBoolean operator<(const char *pString1,const MSString &string2);
  
  friend MSTypesExport MSBoolean operator<=(const MSString &string1,const MSString &string2);
  friend MSTypesExport MSBoolean operator<=(const MSString &string1,const char *pString2);
  friend MSTypesExport MSBoolean operator<=(const char *pString1,const MSString &string2);
  
  friend MSTypesExport MSBoolean operator>(const MSString &string1,const MSString &string2);
  friend MSTypesExport MSBoolean operator>(const MSString &string1,const char *pString2);
  friend MSTypesExport MSBoolean operator>(const char *pString1,const MSString &string2);
  
  friend MSTypesExport MSBoolean operator>=(const MSString &string1,const MSString &string2);
  friend MSTypesExport MSBoolean operator>=(const MSString &string1,const char *pString2);
  friend MSTypesExport MSBoolean operator>=(const char *pString1,const MSString &string2);

/*------------------------------- Conversion -----------------------------------
| The following functions permit the conversion of a string to various other   |
| data types.  The types supported are the same set as are supported by the    |
| MSString constructors:                                                       |
|                                                                              |
|   operator const char*          - Returns a char* pointer to the string's    |
|                                   contents.                                  |
|   operator const unsigned char* - Returns an unsigned char* pointer to the   |
|                                   string's contents.                         |
|   asString                - Returns the string itself                        |
|   asDebugInfo             - Returns information about the MSString's         |
|                             internal representation that can be used for     |
|                             debugging.                                       |
|   asInt                   - Returns, as a long integer, the number that the  |
|                             string represents.                               |
|   asUnsigned              - Returns, as an unsigned long, the number that    |
|                             the string represents.                           |
|   asBoolean               - Returns, as an MSBoolean, the boolean that       |
|                             the string represents - case independent.        |
|   asDouble                - Returns, as a double, the number that the        |
|                             string represents.                               |
|                                                                              |
| In addition, a complete set of functions is provided for converting a        |
| String from any of the following formats to another format:                  |
|   b - A string of binary digits ('0' and '1').                               |
|   c - A normal string of characters.                                         |
|   d - A string of decimal digits ('0' through '9').                          |
|   x - A string of hexadecimal digits ('0' through '9' and 'a/A' through      |
|       'f/F').                                                                |
|                                                                              |
| The following functions work if isBinaryDigits()== MSTrue; if not, they      |
| return a null string:                                                        |
|   b2c - Changes '01' to '\x01', '00110011' to '3'.                           |
|   b2d - Changes '00011001' to '25', '0001001000110100' to '4660'.            |
|   b2x - Changes '00011011' to '1b', '10001001000110100' to '11234'.          |
|                                                                              |
| The following functions always work:                                         |
|   c2b - Changes 'a' to '01010001', '12' to '11000100110010'.                 |
|   c2d - Changes 'a' to '97', 'ab' to '24930'.                                |
|   c2x - Changes 'a' to '61', 'ab' to '6162'.                                 |
|                                                                              |
| The following functions work if isDigits()== MSTrue; if not, they return a   |
| null string:                                                                 |
|   d2b - Changes '12' to '1100', '123' to '1111011'.                          |
|   d2c - Changes '12' to '\x0c', '56' to '8'.                                 |
|   d2x - Changes '12' to 'c', '123' to '7b'.                                  |
|                                                                              |
| The following functions work if isHexDigits()== MSTrue; if not, they return  |
| a null string:                                                               |
|   x2b - Changes 'a1c' to '101000011100', 'f3' to '11110011'.                 |
|   x2c - Changes '8' to '\x08', '31393932' to '1992'.                         |
|   x2d - Changes 'a1c' to '2588', '10000' to '65536'.                         |
|                                                                              |
| Static functions by the same name can be applied to a String to return the   |
| modified string, without changing the argument String.  These functions      |
| are used much like the similar REXX functions.  For example:                 |
|                                                                              |
|   aString.c2b();                                 // Changes aString.         |
|   String binaryDigits= MSString::c2b( aString ); // Leaves aString alone.    |
------------------------------------------------------------------------------*/
  operator const char*() const;
  operator const unsigned char*() const;
  
  virtual MSString asString(void) const;
  virtual MSString asDebugInfo(void) const;
  virtual MSString asMSF(void) const;
  long asInt(void) const; 
  unsigned long asUnsigned(void) const;
  MSBoolean asBoolean(void) const;
  double asDouble(void) const;

  MSString &decodeMSF(void);
  MSString &encodeMSF(void);

  MSString &b2c();
  MSString &b2d();
  MSString &b2x();
  MSString &c2b();
  MSString &c2d();
  MSString &c2x();
  MSString &d2b();
  MSString &d2c();
  MSString &d2x();
  MSString &x2b();
  MSString &x2c();
  MSString &x2d();
  
  static MSString b2c(const MSString &aString);
  static MSString b2d(const MSString &aString);
  static MSString b2x(const MSString &aString);
  static MSString c2b(const MSString &aString);
  static MSString c2d(const MSString &aString);
  static MSString c2x(const MSString &aString);
  static MSString d2b(const MSString &aString);
  static MSString d2c(const MSString &aString);
  static MSString d2x(const MSString &aString);
  static MSString x2b(const MSString &aString);
  static MSString x2c(const MSString &aString);
  static MSString x2d(const MSString &aString);

  const char *format(MSString*) const;
  const char *format(MSString&) const;
  const char *format(MSString*,const MSFormat&) const;
  const char *format(MSString&,const MSFormat&) const;

/*------------------------- Manipulation Operators -----------------------------
| The following functions allow the string's contents to be manipulated.       |
| All are overloaded so that standard C "strings" can be used efficiently,     |
| without constructing an equivalent String first.                             |
|   operator =  - Replaces the contents of the string.                         |
|   operator ~  - Returns bitwise negation (one's complement).                 |
|   operator +  - Concatenates two strings.                                    |
|   operator += - Concatenates and replaces.                                   |
|   operator <<  - Concatenates rhs to lhs - treats lhs as a sink.             |
|   operator <<= - Concatenates and replaces.                                  |
|   operator &  - Performs bitwise AND.                                        |
|   operator &= - Performs bitwise AND and replaces the receiver.              |
|   operator |  - Performs bitwise OR.                                         |
|   operator |= - Performs bitwise OR and replaces the receiver.               |
|   operator ^  - Performs bitwise XOR.                                        |
|   operator ^= - Performs bitwise XOR and replaces the receiver.              |
------------------------------------------------------------------------------*/
  MSString &operator=(const MSString &aString);
  MSString &operator=(const char *pString);
  MSString operator~() const;
  MSString operator+(const MSString &aString) const;
  MSString operator+(const char *pString) const;
  MSString operator+(char aChar) const;
  MSString &operator+=(const MSString &aString);
  MSString &operator+=(const char *pString);
  MSString &operator+=(char aChar);
  MSString &operator<<=(const MSString &aString);
  MSString &operator<<=(const char *pString);
  MSString &operator<<=(char aChar);
  MSString &operator<<(const MSString &aString);
  MSString &operator<<(const char *pString);
  MSString &operator<<(char aChar);
  MSString operator&(const MSString &aString) const;
  MSString operator&(const char *pString) const;
  MSString &operator&=(const MSString &aString);
  MSString &operator&=(const char *pString);
  MSString operator|(const MSString &aString) const;
  MSString operator|(const char *pString) const;
  MSString &operator|=(const MSString &aString);
  MSString &operator|=(const char *pString);
  MSString operator^(const MSString &aString) const;
  MSString operator^(const char *pString) const;
  MSString &operator^=(const MSString &aString);
  MSString &operator^=(const char *pString);

  friend MSTypesExport MSString operator+(char aChar,const MSString &aString);
  friend MSTypesExport MSString operator+(const char *pString,const MSString &aString);
  friend MSTypesExport MSString operator&(const char *pString,const MSString &aString);
  friend MSTypesExport MSString operator|(const char *pString,const MSString &aString);
  friend MSTypesExport MSString operator^(const char *pString,const MSString &aString);

/*------------------------------- Accessors ------------------------------------
| The following functions provide access to general information about the      |
| string:                                                                      |
|   size        - Returns the length of the string, not counting the           |
|                 terminating null character.                                  |
|   length      - Same as size.                                                |
|   subString   - Returns a given substring of the receiver.                   |
|   operator [] - Returns a reference to the nth character of the string.      |
|                 NOTE: The non-const version of this function extends the     |
|                       string if it is invoked with an index beyond the end.  |
|   charType    - Returns the type of the character at the argument index.     |
|   first       - Returns the first character in the string.                   |
|   last        - Returns the last character in the string.                    |
|   className   - Returns the string's class name.                             |
|   type        - Returns the string's symbol type - virtual method.           |
|   symbol      - Returns the string's symbol type - static method.            |
|   stringType  - Returns the string's symbol type - static method.            |
------------------------------------------------------------------------------*/
  const char *string() const;
  unsigned size() const;
  unsigned length() const;
  
  MSString subString(unsigned startPos) const;
  MSString subString(unsigned startPos,unsigned length,char padCharacter=' ') const;

  MSString operator()(unsigned startPos,unsigned length) const;
  MSString operator()(unsigned startPos,unsigned length,char padCharacter) const;

  CharPick operator[](unsigned index);
  const char &operator[](unsigned index) const;

  CharPick operator[](int index);
  const char &operator[](int index) const;

  char operator()(unsigned index);
  char operator()(unsigned index) const;
  
  MSStringEnum::CharType charType(unsigned index) const;

  char first(void) const;
  char last(void) const;
  virtual MSString className(void) const;
  static const MSSymbol& symbol(void);  
  static const MSSymbol& stringType(void);
  virtual const MSSymbol& type(void) const;
  virtual MSModel *clone(void) const;
  virtual MSModel *create(void) const;
  virtual void assign(const MSModel&);
  virtual long compare(const MSModel&) const;

/*------------------------------- Searching ------------------------------------
| The following functions permit searching the string in various ways:         |
|   indexOfAnyBut - Returns the index of the first character of the receiver   |
|                   that is not in the argument set of characters; length() is |
|                   returned if there are no characters.  Alternatively, it    |
|                   returns the index of the first character that fails the    |
|                   test prescribed by an argument MSStringTest object.        |
|   indexOfAnyOf  - Returns the index of the first character of the receiver   |
|                   that is a character in the argument set of characters;     |
|                   length() is returned if there are no characters.           |
|                   Alternatively,                                             |
|                   it returns the index of the first character that passes    |
|                   the test prescribed by an argument MSStringTest object.    |
|   indexOf       - Returns the index of the first occurrence of the           |
|                   argument string within the receiver; length() is returned  | 
|                   if there are no occurrences.  The argument can also be a   |
|                   single character or an MSStringTest object.                |
|   occurrencesOf - Returns the number of occurrences of the argument          |
|                   MSString,char*,char,test.  This is slower than indexOf if  |
|                   you just want a MSBoolean test.                            |
|                                                                              |
| You can specify an optional index that indicates where in the String the     |
| search is to start.  The default is to start at the beginning of the string. |
|                                                                              |
| Each of the indexing functions also has a lastIndexOf version that returns   |
| the index of the last character in the receiver MSString that satisfies the  |
| search criteria.  These functions accept an optional argument that           |
| specifies where the search is to begin.  The default is to begin searching   |
| at the end of the string.  Searching proceeds from right to left for these   |
| functions.                                                                   |
|                                                                              |
|   lastIndexOf       - Returns the index of the last occurrence of the        |
|                       argument.                                              |
|   lastIndexOfAnyBut - Returns the index of the last character not in the     |
|                       argument.                                              |
|   lastIndexOfAnyOf  - Returns the index of the last character in the         |
|                       argument.                                              |
------------------------------------------------------------------------------*/
  unsigned indexOf(const MSString &aString,unsigned startPos=0) const;
  unsigned indexOf(const char *pString,unsigned startPos=0) const;
  unsigned indexOf(char aCharacter,unsigned startPos=0) const;
  unsigned indexOf(const MSStringTest &aTest,unsigned startPos=0) const;
  
  unsigned indexOfAnyBut(const MSString &validChars,unsigned startPos=0) const;
  unsigned indexOfAnyBut(const char *pValidChars,unsigned startPos=0) const;
  unsigned indexOfAnyBut(char validChar,unsigned startPos=0) const;
  unsigned indexOfAnyBut(const MSStringTest &aTest,unsigned startPos=0) const;
  
  unsigned indexOfAnyOf(const MSString &searchChars,unsigned startPos=0) const;
  unsigned indexOfAnyOf(const char *pSearchChars,unsigned startPos=0) const;
  unsigned indexOfAnyOf(char searchChar,unsigned startPos=0) const;
  unsigned indexOfAnyOf(const MSStringTest &aTest,unsigned startPos=0) const;
  
  unsigned lastIndexOf(const MSString &aString,unsigned startPos=UINT_MAX-1) const;
  unsigned lastIndexOf(const char *pString,unsigned startPos=UINT_MAX-1) const;
  unsigned lastIndexOf(char aCharacter,unsigned startPos=UINT_MAX-1) const;
  unsigned lastIndexOf(const MSStringTest &aTest,unsigned startPos=UINT_MAX-1) const;
  
  unsigned lastIndexOfAnyBut(const MSString &validChars,unsigned startPos=UINT_MAX-1) const;
  unsigned lastIndexOfAnyBut(const char *pValidChars,unsigned startPos=UINT_MAX-1) const;
  unsigned lastIndexOfAnyBut(char validChar,unsigned startPos=UINT_MAX-1) const;
  unsigned lastIndexOfAnyBut(const MSStringTest &aTest,unsigned startPos=UINT_MAX-1) const;
  
  unsigned lastIndexOfAnyOf(const MSString &searchChars,unsigned startPos=UINT_MAX-1) const;
  unsigned lastIndexOfAnyOf(const char *pSearchChars,unsigned startPos=UINT_MAX-1) const;
  unsigned lastIndexOfAnyOf(char searchChar,unsigned startPos=UINT_MAX-1) const;
  unsigned lastIndexOfAnyOf(const MSStringTest &aTest,unsigned startPos=UINT_MAX-1) const;
  
  unsigned occurrencesOf(const MSString &aString,unsigned startPos=0) const;
  unsigned occurrencesOf(const char *pString,unsigned startPos=0) const;
  unsigned occurrencesOf(char aCharacter,unsigned startPos=0) const;
  unsigned occurrencesOf(const MSStringTest &aTest,unsigned startPos=0) const;

/*-------------------------------- Editing -------------------------------------
| The following functions are used to edit the string.  All return a           |
| reference to the modified receiver.  Many that are length related, such as   |
| center and leftJustify, accept a pad character that defaults to a blank.     |
| In all cases, argument strings can be specified as either objects of the     |
| MSString class or by using char*.                                            |
|                                                                              |
| Static functions by the same name can be applied to an MSString to obtain    |
| the modified MSString without affecting the argument.  For example:          |
|                                                                              |
| aString.change('\t','   '); // Changes all tabs in aString to 3 blanks.      |
| MSString s= MSString::change(aString,'\t', '   '); // Leaves aString as is.  |
|                                                                              |
|   center              - Centers the receiver within a string of the          |
|                         specified length.                                    |
|   change              - Changes occurrences of an argument pattern to an     |
|                         argument replacement string.  The number of changes  |
|                         to perform can be specified.  The default is to      |
|                         change all occurrences of the pattern.  You can      |
|                         also specify the position in the receiver at which   |
|                         to begin.                                            |
|   copy                - Replaces the receiver's contents with a given        |
|                         number of replications of itself.                    |
|   remove              - Deletes the specified substring from the receiver.   |
|   insert              - Inserts an argument string at a given location.      |
|   leftJustify         - Left-justifies the receiver in a string of the       |
|                         specified length.                                    |
|   lowerCase           - Translates all upper case letters in the receiver    |
|                         to lower case.                                       |
|   overlayWith         - Replaces a given portion of the receiver's contents  |
|                         with an argument string.                             |
|   reverse             - Reverses the receiver's contents.                    |
|   rightJustify        - Right-justifies the receiver in a string of the      |
|                         specified length.                                    |
|   strip               - Strips both leading and trailing character or        |
|                         characters; the character or characters can be       |
|                         specified either as a single char, a String or       |
|                         char* array, or with an MSStringTest object.  The    |
|                         default is whitespace.                               |
|   stripBlanks         - Static function that strips both leading and         |
|                         trailing whitespace.  This function is the static    |
|                         version of strip (renamed to avoid duplicate         |
|                         definition).                                         |
|   stripLeading        - Strips the leading character or characters.          |
|   stripLeadingBlanks  - Static version of stripLeading (renamed to avoid     |
|                         duplicate definition).                               |
|   stripTrailing       - Strips the trailing character or characters.         |
|   stripTrailingBlanks - Static version of stripTrailing (renamed to avoid    |
|                         duplicate definition).                               |
|   translate           - Converts all of the receiver's characters that are   |
|                         in one argument string to the corresponding          |
|                         character in a second argument string.               |
|   upperCase           - Translates all lower case letters in the receiver    |
|                         to upper case.                                       |
------------------------------------------------------------------------------*/
  MSString &center(unsigned length,char padCharacter=' ');
  
  MSString &change(const MSString &inputString,const MSString &outputString,
		   unsigned startPos=0,unsigned numChanges=UINT_MAX);
  MSString &change(const MSString &inputString,const char *pOutputString,
		   unsigned startPos=0,unsigned numChanges=UINT_MAX);
  MSString &change(const char *pInputString,const MSString &outputString,
		   unsigned startPos=0,unsigned numChanges=UINT_MAX);
  MSString &change(const char *pInputString,const char *pOutputString,
		   unsigned startPos=0,unsigned numChanges=UINT_MAX);
  
  MSString &copy(unsigned numCopies);
  
  MSString &insert(const MSString &aString,unsigned index=0,char padCharacter=' ');
  MSString &insert(const char *pString,unsigned index=0,char padCharacter=' ');
  
  MSString &leftJustify(unsigned length,char padCharacter=' ');
  
  MSString &lowerCase();
  MSString &upperCase();
  
  MSString &overlayWith(const MSString &aString,unsigned index=0,char padCharacter=' ');
  MSString &overlayWith(const char *pString,unsigned index=0,char padCharacter=' ');
  
  MSString &remove(unsigned startPos);
  MSString &remove(unsigned startPos,unsigned numChars);
  
  MSString &reverse();
  
  MSString &rightJustify(unsigned length,char padCharacter=' ');
  
  MSString &strip();
  MSString &strip(char aCharacter);
  MSString &strip(const MSString &aString);
  MSString &strip(const char *pString);
  MSString &strip(const MSStringTest &aTest);
  
  MSString &stripLeading();
  MSString &stripLeading(char aCharacter);
  MSString &stripLeading(const MSString &aString);
  MSString &stripLeading(const char *pString);
  MSString &stripLeading(const MSStringTest &aTest);
  
  MSString &stripTrailing();
  MSString &stripTrailing(char aCharacter);
  MSString &stripTrailing(const MSString &aString);
  MSString &stripTrailing(const char *pString);
  MSString &stripTrailing(const MSStringTest &aTest);
  
  MSString &translate(const MSString &inputChars,const MSString &outputChars,char padCharacter=' ');
  MSString &translate(const MSString &inputChars,const char *pOutputChars,char padCharacter=' ');
  MSString &translate(const char *pInputChars,const MSString &outputChars,char padCharacter=' ');
  MSString &translate(const char *pInputChars,const char *pOutputChars,char padCharacter=' ');
  
  static MSString center(const MSString &aString,unsigned length,char padCharacter=' ');
  
  static MSString change(const MSString &aString,const MSString &inputString,
			 const MSString &outputString,unsigned startPos=0,unsigned numChanges=UINT_MAX);
  static MSString change(const MSString &aString,const MSString &inputString,
			 const char *pOutputString,unsigned startPos=0,unsigned numChanges=UINT_MAX);
  static MSString change(const MSString &aString,const char *pInputString,
			 const MSString &outputString,unsigned startPos=0,unsigned numChanges=UINT_MAX);
  static MSString change(const MSString &aString,const char *pInputString,
			 const char *pOutputString,unsigned startPos=0,unsigned numChanges=UINT_MAX);
  
  static MSString copy(const MSString &aString,unsigned numCopies);
  
  static MSString insert(const MSString &aString,const MSString &anInsert,unsigned index=0,char padCharacter=' ');
  static MSString insert(const MSString &aString,const char *pInsert,unsigned index=0,char padCharacter=' ');
  
  static MSString leftJustify(const MSString &aString,unsigned length,char padCharacter=' ');
  
  static MSString lowerCase(const MSString &aString);
  
  static MSString overlayWith(const MSString &aString,const MSString &anOverlay,unsigned index=0,char padCharacter=' ');
  static MSString overlayWith(const MSString &aString,const char *pOverlay,unsigned index=0,char padCharacter=' ');
  
  static MSString remove(const MSString &aString,unsigned startPos);
  static MSString remove(const MSString &aString,unsigned startPos,unsigned numChars);
  
  static MSString reverse(const MSString &aString);
  
  static MSString rightJustify(const MSString &aString,unsigned length,char padCharacter=' ');
  
  static MSString stripBlanks(const MSString &aString);
  static MSString strip(const MSString &aString,char aChar);
  static MSString strip(const MSString &aString,const MSString &aStringOfChars);
  static MSString strip(const MSString &aString,const char *pStringOfChars);
  static MSString strip(const MSString &aString,const MSStringTest &aTest);
  
  static MSString stripLeadingBlanks(const MSString &aString);
  static MSString stripLeading(const MSString &aString,char aChar);
  static MSString stripLeading(const MSString &aString,const MSString &aStringOfChars);
  static MSString stripLeading(const MSString &aString,const char *pStringOfChars);
  static MSString stripLeading(const MSString &aString,const MSStringTest &aTest);
  
  static MSString stripTrailingBlanks(const MSString &aString);
  static MSString stripTrailing(const MSString &aString,char aChar);
  static MSString stripTrailing(const MSString &aString,const MSString &aStringOfChars);
  static MSString stripTrailing(const MSString &aString,const char *pStringOfChars);
  static MSString stripTrailing(const MSString &aString,const MSStringTest &aTest);
  
  static MSString translate(const MSString &aString,const MSString &inputChars,
			   const MSString &outputChars,char padCharacter=' ');
  static MSString translate(const MSString &aString,const MSString &inputChars,
			   const char *pOutputChars,char padCharacter=' ');
  static MSString translate(const MSString &aString,const char *pInputChars,
			   const MSString &outputChars,char padCharacter=' ');
  static MSString translate(const MSString &aString,const char *pInputChars,
			   const char *pOutputChars,char padCharacter=' ');
  static MSString upperCase(const MSString &aString);

  virtual MSError::ErrorStatus set(const char *pString);
  virtual MSError::ErrorStatus setFromMSF(const char *pString);
  virtual MSError::ErrorStatus set(unsigned index,char aChar);
  virtual MSError::ErrorStatus set(const MSString *pString);
  virtual MSError::ErrorStatus set(const MSString &aString);
  MSBoolean isSet() const;

  MSString& truncate(unsigned len=0);
  MSString& exchange(unsigned index1,unsigned index2);

  MSString& removeAll();
  MSString& lower();
  MSString& upper();
  MSString& rotate(int count);
  MSString& take(int count);
  MSString& drop(int count);

  friend MSTypesExport MSString lower(const MSString& aString);
  friend MSTypesExport MSString upper(const MSString& aString);
  friend MSTypesExport MSString rotate(const MSString& aString,int count);
  friend MSTypesExport MSString take(const MSString& aString,int count);
  friend MSTypesExport MSString drop(const MSString& aString,int count);

/*----------------------------- Word Functions ---------------------------------
| The following functions operate on the string as a collection of words       |
| separated by whitespace characters:                                          |
|                                                                              |
|   indexOfPhrase     - Returns the position of the first occurrence of the    |
|                       argument phrase in the receiver.  Returns 0 if the     |
|                       phrase is not found.                                   |
|   indexOfWord       - Returns the index of the nth whitespace-delimited      |
|                       word in the receiver.  Returns length() if the word is not    |
|                       found.                                                 |
|   lengthOfWord      - Returns the length of the nth whitespace-delimited     |
|                       word in the receiver.                                  |
|   numWords          - Returns the number of whitespace-delimited words in    |
|                       the receiver.                                          |
|   removeWords       - Deletes the specified words from the receiver's        |
|                       contents; the words are specified by using a starting  |
|                       word number and the number of words.  The latter       |
|                       defaults to the rest of the string.                    |
|                       NOTE: The static functions space and removeWords       |
|                             obtain the same result, but do not affect the    |
|                             String to which they are applied.                |
|   space             - Modifies the receiver so that all words are separated  |
|                       by the specified number of blanks.  The default is     |
|                       one blank.  All whitespace is converted to simple      |
|                       blanks.                                                |
|                       NOTE: The static functions space and removeWords       |
|                             obtain the same result, but do not affect the    |
|                             String to which they are applied.                |
|   word              - Returns a copy of the nth whitespace-delimited word    |
|                       in the receiver.                                       |
|   wordIndexOfPhrase - Returns the word number of the first word in the       |
|                       receiver that matches the argument phrase.  It starts  |
|                       searching with the word number in the startWord        |
|                       argument, which defaults to 0.  It returns word count  |
|                       if the phrase is not found.                            |
|   words             - Returns a substring of the receiver that starts at a   |
|                       given word and is comprised of a given number of       |
|                       words; the word separators are copied to the result    |
|                       intact.                                                |
------------------------------------------------------------------------------*/
  MSString &removeWords(unsigned firstWord);
  MSString &removeWords(unsigned firstWord,unsigned numWords);
  MSString &space(unsigned numSpaces=1,char spaceChar=' ');
  MSString word(unsigned wordNumber) const;
  MSString words(unsigned firstWord) const;
  MSString words(unsigned firstWord,unsigned numWords) const;
  
  unsigned indexOfPhrase(const MSString &wordString,unsigned startWord=0) const;
  unsigned indexOfWord(unsigned wordNumber) const;
  unsigned lengthOfWord(unsigned wordNumber) const;
  unsigned numWords() const;
  unsigned wordIndexOfPhrase(const MSString &aPhrase,unsigned startWord=0) const;
  
  static MSString space(const MSString &aString,unsigned numSpaces=1,char spaceChar=' ');
  static MSString removeWords(const MSString &aString,unsigned startWord);
  static MSString removeWords(const MSString &aString,unsigned startWord,unsigned numWords);

protected:
/*------------------------------ Related Types ---------------------------------
  The following enumeration types are defined to pass "modes" to various
  implementation functions:
    BitOperator - Enumeration that specifies the bit operator to apply to the
                  applyBitOp function.  Valid bit operators are 'and', 'or',
                  and 'exclusiveOr'.
    IndexType   - Enumeration that specifies whether the result from the
                  findPhrase function is a word index or a character index.
                  The valid values are:
                    charIndex - Returns the result as the byte index within
                                the string.
                    wordIndex - Returns the result as the index of the
                                matching word.  For example, the first word
                                is 1, the second word is 2, and so forth.
------------------------------------------------------------------------------*/
  enum BitOperator {And,Or,ExclusiveOr};
  enum IndexType {CharIndex,WordIndex};

/*--------------------------- Static Data Members ------------------------------
  The following are static data members:
    null          - A string that contains no element.
    maxLong       - The maximum value of a long, which is "2147483647" on
                    OS/2 with 32-bit unsigned long integers.
------------------------------------------------------------------------------*/
  static const char *null;
  static const char *maxLong;

/*------------------------------ Implementation --------------------------------
  The following functions are used to implement this class:
    data          - Returns the address of the contents of the MSString.
    setBuffer     - Sets the private data member to point to a new MSStringBuffer
                    object.
    buffer        - Returns the address of the MSStringBuffer referred to by this
                    MSString.
    lengthOf      - Returns the length of a plain C character array.
    initBuffer    - Resets the contents from an argument buffer or buffers.
    defaultBuffer - Returns a pointer to the MSStringBuffer::defaultBuffer.

  The following are common implementations of various overloaded versions
  of similarly named public functions:
    occurrencesOf - See MSString::occurrencesOf.
    isLike        - See MSString::isLike.
    isAbbrevFor   - See MSString::isAbbrevFor.
    change        - See MSString::change.
    insert        - See MSString::insert.
    overlayWith   - See MSString::overlayWith.
    strip         - See MSString::strip.
    translate     - See MSString::translate.
    indexOfWord   - See MSString::indexOfWord.

  The following are used to implement various public functions of this class:
    applyBitOp  - Implements bitwise operators &, |, and ^.
    findPhrase  - Locates a given string of words for indexOfWord functions.
------------------------------------------------------------------------------*/
  char *data() const;
  MSStringBuffer *buffer() const;
  
  static unsigned lengthOf(const char *p);
  static MSStringBuffer *defaultBuffer();
  
  MSString &setBuffer(MSStringBuffer *ibuff);
  
  MSString &initBuffer(const void *p1,unsigned len1,
		       const void *p2=0,unsigned len2=0,
		       const void *p3=0,unsigned len3=0,char padChar=0);
  MSString &initBuffer(long n);
  MSString &initBuffer(unsigned long n);
  MSString &initBuffer(double d);
  MSString &initBuffer(void *p_);
  
  MSString &applyBitOp(const char *pArg,unsigned argLen,BitOperator op);
  
  MSString &change(const char *pPattern, unsigned patternLen,
		   const char *pReplacement,unsigned replacementLen,unsigned startPos,unsigned numChanges);
  
  MSString &insert(const char *pInsert,unsigned insertLen,unsigned startPos,char padCharacter);
  MSString &overlayWith(const char *pOverlay,unsigned overlayLen,unsigned index,char padCharacter);
  MSString &strip(const char *p,unsigned len,MSStringEnum::StripMode mode);
  MSString &strip(const MSStringTest &aTest,MSStringEnum::StripMode mode);
  MSString &translate(const char *pInputChars,unsigned inputLen,
		      const char *pOutputChars,unsigned outputLen,char padCharacter);
  
  unsigned indexOfWord(unsigned wordNumber,unsigned startPos,unsigned numWords) const;
  unsigned occurrencesOf(const char *pSearchString,unsigned searchLen,unsigned startPos) const;
  unsigned findPhrase(const MSString &aPhrase,unsigned startWord,IndexType charOrWord) const;
  
  MSBoolean isLike(const char *pPattern,unsigned patternLen,char zeroOrMore,char anyChar) const;
  MSBoolean isAbbrevFor(const char *pFullString,unsigned fullLen,unsigned minLen) const;

private: /*------------------------ PRIVATE ----------------------------------*/
friend class MSStringBuffer;
friend class CharPick;

  MSString(MSStringBuffer *pBuffer);
  MSString &prepareToChange();
  void binaryMath(unsigned char newDigit);
  void decimalMath(unsigned char newDigit);
  
  MSStringBuffer  *_pBuffer;
};


#ifndef MSStringTestHEADER
#include <MSTypes/MSStringTest.H>
#endif 

#ifndef MS_NO_INLINES
#ifndef MSStringINLINES
#include <MSTypes/MSStringInlines.C>
#endif
#endif

/*------------------------ Comparison Function ------------------------*/
inline long compare(const MSString &aString_,const MSString &bString_)
{ return aString_.compare(bString_); }

/*--------------------------- Hash Function ---------------------------*/
inline unsigned long hash(const MSString& aString_,unsigned long size_)
{
  char *pString=aString_.data();
  unsigned long h=0,c;
  while ((c=(unsigned long)*pString++)!=0) h+=(h<<5)+c;
  return h%size_;
}

#if defined (MS_SPECIALIZE_ELEMENT_FOR_OPS_TYPE)
#include <MSTypes/MSStandardOps.H>
DECLARE_ELEMENT_FOR_OPS_TYPE(MSString)
#endif
#endif







