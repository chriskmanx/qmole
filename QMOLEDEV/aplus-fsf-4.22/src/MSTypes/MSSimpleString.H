#ifndef MSSimpleStringHEADER
#define MSSimpleStringHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSEnum.H>

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif


class MSTypesExport MSSimpleString
{
public:

  MSSimpleString(void);
  MSSimpleString(const char *);
  MSSimpleString(const MSSimpleString&);
  MSSimpleString(const MSSimpleString&,const MSSimpleString&);
  ~MSSimpleString(void);

  operator const char*() const;

  const char *string(void) const;
  unsigned length(void) const;

  MSSimpleString& operator=(const MSSimpleString&);
  MSSimpleString& operator=(const char *);
  MSSimpleString& operator=(char);

  MSSimpleString& operator<<=(const MSSimpleString&);
  MSSimpleString& operator<<=(const char *);
  MSSimpleString& operator<<=(char);
  MSSimpleString& operator<< (const MSSimpleString&);
  MSSimpleString& operator<< (const char *);
  MSSimpleString& operator<< (char);

  MSSimpleString& operator+=(const MSSimpleString&);
  MSSimpleString& operator+=(const char *);
  MSSimpleString& operator+=(char);

  MSBoolean operator==(const MSSimpleString&) const;
  MSBoolean operator!=(const MSSimpleString&) const;

  char operator[](unsigned) const;
  char& operator[](unsigned);
  char operator()(unsigned) const;
  char& operator()(unsigned);

  friend MSTypesExport MSSimpleString operator+(const MSSimpleString&,const MSSimpleString&); 
  friend MSTypesExport MSSimpleString operator+(const MSSimpleString&,const char *); 
  friend MSTypesExport MSSimpleString operator+(const char *,const MSSimpleString&); 
  friend MSTypesExport MSSimpleString operator+(const MSSimpleString&,char); 
  friend MSTypesExport MSSimpleString operator+(char,const MSSimpleString&); 

  friend MSTypesExport ostream& operator<<(ostream&,const MSSimpleString&);

protected:
  static char  _badData;   // used as return value for index errors
  char        *_string;
  unsigned     _length;

  void indexError(unsigned,unsigned) const; 

  void duplicate(const char *);
  void string(const char *);
  MSSimpleString& catenate(const char *);

private: 
         // special private constructors for catenation - take advantage of 
         // RVO - return value optimization - see 12.1c of the ARM
  MSSimpleString(const MSSimpleString&,const char *);
  MSSimpleString(const char *,const MSSimpleString&);
  MSSimpleString(const MSSimpleString&,char);
  MSSimpleString(char,const MSSimpleString&);
};

inline MSSimpleString::operator const char*() const
{ return _string; }
inline const char *MSSimpleString::string(void) const  
{ return _string; }
inline unsigned MSSimpleString::length(void) const    
{ return _length; }

inline char MSSimpleString::operator[](unsigned i_) const 
{ return (i_<_length)?_string[i_]:(indexError(i_,length()),_badData); }
inline char& MSSimpleString::operator[](unsigned i_)
{ if (i_<_length) return _string[i_];
  else return (indexError(i_,length()),_badData); }

inline char MSSimpleString::operator()(unsigned i_) const 
{ if (i_<_length) return _string[i_];
  else return (indexError(i_,length()),_badData); }
inline char& MSSimpleString::operator()(unsigned i_)
{ if (i_<_length) return _string[i_];
  else return (indexError(i_,length()),_badData); }

inline MSSimpleString& MSSimpleString::operator<<=(const MSSimpleString& aString) 
{ return catenate(aString.string()); }
inline MSSimpleString& MSSimpleString::operator<<=(const char *pString)           
{ return catenate(pString); }

inline MSSimpleString& MSSimpleString::operator+=(const MSSimpleString& aString) 
{ return *this<<=aString; }
inline MSSimpleString& MSSimpleString::operator+=(const char *pString)           
{ return *this<<=pString; }
inline MSSimpleString& MSSimpleString::operator+=(char aChar)                  
{ return *this<<=aChar; }

#endif
